! The main program which runs our driver test case potentials
!
! Copyright (C) 2013, Joshua More and Michele Ceriotti
!
! Permission is hereby granted, free of charge, to any person obtaining
! a copy of this software and associated documentation files (the
! "Software"), to deal in the Software without restriction, including
! without limitation the rights to use, copy, modify, merge, publish,
! distribute, sublicense, and/or sell copies of the Software, and to
! permit persons to whom the Software is furnished to do so, subject to
! the following conditions:
!
! The above copyright notice and this permission notice shall be included
! in all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
! EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
! MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
! IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
! CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
! TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
! SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!
!
! Currently the potentials implemented are the Lennard-Jones
! potential, the Silvera-Goldman para-hydrogen potential and
! the ideal gas (i.e. no interaction at all)

      PROGRAM DRIVER
         USE pes_shell
         USE DISTANCE, only: CELL_VOLUME
         USE F90SOCKETS, ONLY : open_socket, writebuffer, readbuffer, f_sleep
      IMPLICIT NONE
      
      ! SOCKET VARIABLES
      INTEGER, PARAMETER :: MSGLEN=12   ! length of the headers of the driver/wrapper communication protocol
      INTEGER socket, inet, port        ! socket ID & address of the server
      CHARACTER(LEN=1024) :: host, sockets_prefix='/tmp/ipi_'
      
      ! COMMAND LINE PARSING
      CHARACTER(LEN=1024) :: cmdbuffer
      INTEGER ccmd, vstyle, vseed
      INTEGER, ALLOCATABLE :: seed(:)
      INTEGER verbose
      INTEGER commas(4), par_count      ! stores the index of commas in the parameter string
      DOUBLE PRECISION vpars(6)         ! array to store the parameters of the potential
      
      ! SOCKET COMMUNICATION BUFFERS
      CHARACTER(LEN=12) :: header
      LOGICAL :: isinit=.false., hasdata=.false.
      INTEGER cbuf, rid, length
      CHARACTER(LEN=65536) :: initbuffer      ! it's unlikely a string this large will ever be passed...
      CHARACTER(LEN=65536) :: string,string1,string2,string3,trimmed  ! it's unlikely a string this large will ever be passed...
      CHARACTER(LEN=30000) :: longbuffer, longstring ! used in water_dip_pol model to pass dipole-z derivative and polarizability
      DOUBLE PRECISION, ALLOCATABLE :: msgbuffer(:)
      
      ! PARAMETERS OF THE SYSTEM (CELL, ATOM POSITIONS, ...)
      DOUBLE PRECISION sigma, eps, rc1, rn, ks ! potential parameters
      DOUBLE PRECISION stiffness ! lennard-jones polymer
      DOUBLE PRECISION sleep_seconds
      INTEGER n_monomer ! lennard-jones polymer
      INTEGER nat
      DOUBLE PRECISION pot, dpot, dist
      DOUBLE PRECISION, ALLOCATABLE :: atoms(:,:), forces(:,:), datoms(:,:)
      DOUBLE PRECISION cell_h(3,3), cell_ih(3,3), virial(3,3), mtxbuf(9), dip(3), charges(3), dummy(3,3,3), vecdiff(3)
      DOUBLE PRECISION, ALLOCATABLE :: friction(:,:)
      DOUBLE PRECISION volume
      DOUBLE PRECISION, PARAMETER :: fddx = 1.0d-5
 
      DOUBLE PRECISION, ALLOCATABLE :: dipz_der(:, :) ! Dipole (z-component) derivative (water_dip_pol model)
      DOUBLE PRECISION :: pol(3, 3) !Polarizability (water_dip_pol model)

      ! NEIGHBOUR LIST ARRAYS
      INTEGER, DIMENSION(:), ALLOCATABLE :: n_list, index_list
      DOUBLE PRECISION init_volume, init_rc ! needed to correctly adjust the cut-off radius for variable cell dynamics
      DOUBLE PRECISION, ALLOCATABLE :: last_atoms(:,:) ! Holds the positions when the neighbour list is created
      DOUBLE PRECISION displacement ! Tracks how far each atom has moved since the last call of nearest_neighbours

      ! DMW
      DOUBLE PRECISION efield(3)
      INTEGER i, j

      ! Qi: additional variables for q-AQUA-pol
      DOUBLE PRECISION :: box(3)
      INTEGER :: natm
      real::tmpx(768,3),tmpgd(768,3),pot1,pot2,delta1,tmpbox(3),tmpvirial(3,3)
      
      ! parse the command line parameters
      ! intialize defaults
      ccmd = 0
      inet = 1
      host = "localhost"//achar(0)
      port = 31415
      verbose = 0
      par_count = 0
      vstyle = -1
      rc1 = 0.0d0
      init_rc = 0.0d0
      volume = 0.0d0
      init_volume = 0.0d0

      DO i = 1, COMMAND_ARGUMENT_COUNT()
         CALL GET_COMMAND_ARGUMENT(i, cmdbuffer)
         IF (cmdbuffer == "-u") THEN ! flag for unix socket
            inet = 0
            ccmd = 0
         ELSEIF (cmdbuffer == "-h") THEN ! read the hostname (deprecated)
            ccmd = 1
         ELSEIF (cmdbuffer == "-a") THEN ! read the hostname (address)
            ccmd = 1
         ELSEIF (cmdbuffer == "-p") THEN ! reads the port number
            ccmd = 2
         ELSEIF (cmdbuffer == "-m") THEN ! reads the style of the potential function
            ccmd = 3
         ELSEIF (cmdbuffer == "-o") THEN ! reads the parameters
            ccmd = 4
         ELSEIF (cmdbuffer == "-v") THEN ! flag for verbose standard output
            verbose = 1
         ELSEIF (cmdbuffer == "-vv") THEN ! flag for verbose standard output
            verbose = 2
         ELSE
            IF (ccmd == 0) THEN
               WRITE(*,*) " Unrecognized command line argument", ccmd
               CALL helpmessage
               STOP "ENDED"
            ENDIF
            IF (ccmd == 1) THEN
               host = trim(cmdbuffer)//achar(0)
            ELSEIF (ccmd == 2) THEN
               READ(cmdbuffer,*) port
            ELSEIF (ccmd == 3) THEN
               IF (verbose>0) THEN
                  WRITE(*,*) "Running potential type ", trim(cmdbuffer)
               ENDIF
               IF (trim(cmdbuffer) == "q-aqua-pol") THEN
                  vstyle = 1
               ELSEIF (trim(cmdbuffer) == "dummy") THEN
                  vstyle = 99 ! returns non-zero but otherwise meaningless values
               ELSE
                  WRITE(*,*) " Unrecognized potential type ", trim(cmdbuffer)
                  WRITE(*,*) " Use -m [dummy|q-aqua-pol] "
                  STOP "ENDED"
               ENDIF
            ELSEIF (ccmd == 4) THEN
               par_count = 1
               commas(1) = 0
               DO WHILE (index(cmdbuffer(commas(par_count)+1:), ',') > 0)
                  commas(par_count + 1) = index(cmdbuffer(commas(par_count)+1:), ',') + commas(par_count)
                  READ(cmdbuffer(commas(par_count)+1:commas(par_count + 1)-1),*) vpars(par_count)
                  par_count = par_count + 1
               ENDDO
               READ(cmdbuffer(commas(par_count)+1:),*) vpars(par_count)
            ELSEIF (ccmd == 5) THEN
               sockets_prefix = trim(cmdbuffer)//achar(0)
            ENDIF
            ccmd = 0
         ENDIF
      ENDDO
      
      IF (vstyle == -1) THEN
         WRITE(*,*) " Error, type of potential not specified."
         CALL helpmessage
         STOP "ENDED"
      ELSEIF (99 == vstyle) THEN
         IF (par_count == 0) THEN
            sleep_seconds = 0.0
         ELSEIF (par_count == 1) THEN
            sleep_seconds = vpars(1)
         ELSE
            WRITE(*,*) "Error: only an optional delay parameters needed for dummy output."
            STOP "ENDED"
         ENDIF
         CALL RANDOM_SEED(size=vseed)
         ALLOCATE(seed(vseed))
         seed = 12345
         CALL RANDOM_SEED(put=seed)
         isinit = .true.         
      ELSEIF (1 == vstyle) THEN
         IF (par_count /= 0) THEN
            WRITE(*,*) "Error: no initialization string needed."
            STOP "ENDED"
         ENDIF
         natm = 768
         nw = natm/3
         call pes_init(nw) 
         isinit = .true.
      ENDIF

      IF (verbose > 0) THEN
         WRITE(*,*) " DRIVER - Connecting to host ", trim(host)
         IF (inet > 0) THEN
            WRITE(*,*) " on port ", port, " using an internet socket."
         ELSE
            WRITE(*,*) " using an UNIX socket."
         ENDIF
      ENDIF

      ! Calls the interface to the POSIX sockets library to open a communication channel
      CALL open_socket(socket, inet, port, host, sockets_prefix)
      nat = -1
      DO WHILE (.true.) ! Loops forever (or until the wrapper ends!)

         ! Reads from the socket one message header
         CALL readbuffer(socket, header, MSGLEN)
         IF (verbose > 0) WRITE(*,*) " Message from server: ", trim(header)

         IF (trim(header) == "STATUS") THEN
            ! The wrapper is inquiring on what we are doing
            IF (.not. isinit) THEN
               CALL writebuffer(socket,"NEEDINIT    ",MSGLEN)  ! Signals that we need initialization data
               IF (verbose > 1) WRITE(*,*) "    !write!=> ", "NEEDINIT    "
            ELSEIF (hasdata) THEN
               CALL writebuffer(socket,"HAVEDATA    ",MSGLEN)  ! Signals that we are done computing and can return forces
               IF (verbose > 1) WRITE(*,*) "    !write!=> ", "HAVEDATA    "
            ELSE
               CALL writebuffer(socket,"READY       ",MSGLEN)  ! We are idling and eager to compute something
               IF (verbose > 1) WRITE(*,*) "    !write!=> ", "READY       "
            ENDIF
         ELSEIF (trim(header) == "INIT") THEN     ! The driver is kindly providing a string for initialization
            CALL readbuffer(socket, rid)
            IF (verbose > 1) WRITE(*,*) "    !read!=> RID: ", rid
            CALL readbuffer(socket, cbuf)
            IF (verbose > 1) WRITE(*,*) "    !read!=> init_length: ", cbuf
            CALL readbuffer(socket, initbuffer, cbuf)
            IF (verbose > 1) WRITE(*,*) "    !read!=> init_string: ", cbuf
            IF (verbose > 0) WRITE(*,*) " Initializing system from wrapper, using ", trim(initbuffer)
            isinit=.true. ! We actually do nothing with this string, thanks anyway. Could be used to pass some information (e.g. the input parameters, or the index of the replica, from the driver
         ELSEIF (trim(header) == "POSDATA") THEN  ! The driver is sending the positions of the atoms. Here is where we do the calculation!

            ! Parses the flow of data from the socket
            CALL readbuffer(socket, mtxbuf, 9)  ! Cell matrix
            IF (verbose > 1) WRITE(*,*) "    !read!=> cell: ", mtxbuf
            cell_h = RESHAPE(mtxbuf, (/3,3/))
            CALL readbuffer(socket, mtxbuf, 9)  ! Inverse of the cell matrix (so we don't have to invert it every time here)
            IF (verbose > 1) WRITE(*,*) "    !read!=> cell-1: ", mtxbuf
            cell_ih = RESHAPE(mtxbuf, (/3,3/))

            ! The wrapper uses atomic units for everything, and row major storage.
            ! At this stage one should take care that everything is converted in the
            ! units and storage mode used in the driver.
            cell_h = transpose(cell_h)
            cell_ih = transpose(cell_ih)
            ! We compute for a generic cell, just in case (even though usually i-PI passes an upper triangular cell-vector matrix)
            volume = CELL_VOLUME(cell_h) !cell_h(1,1)*cell_h(2,2)*cell_h(3,3)

            CALL readbuffer(socket, cbuf)       ! The number of atoms in the cell
            IF (verbose > 1) WRITE(*,*) "    !read!=> cbuf: ", cbuf
            IF (nat < 0) THEN  ! Assumes that the number of atoms does not change throughout a simulation, so only does this once
               nat = cbuf
               IF (verbose > 0) WRITE(*,*) " Allocating buffer and data arrays, with ", nat, " atoms"
               ALLOCATE(msgbuffer(3*nat))
               ALLOCATE(atoms(nat,3), datoms(nat,3))
               ALLOCATE(forces(nat,3))
               ALLOCATE(friction(3*nat,3*nat))
               atoms = 0.0d0
               datoms = 0.0d0
               forces = 0.0d0
               friction = 0.0d0
               msgbuffer = 0.0d0
               IF (verbose > 1) WRITE(*,*) " Allocation successful "
            ENDIF

            CALL readbuffer(socket, msgbuffer, nat*3)
            IF (verbose > 1) WRITE(*,*) "    !read!=> positions: ", msgbuffer(0:2), " ..."
            DO i = 1, nat
               atoms(i,:) = msgbuffer(3*(i-1)+1:3*i)
            ENDDO

            IF (vstyle == 99) THEN ! dummy output, useful to test that i-PI "just runs"
               IF (sleep_seconds > 0) THEN
                  ! artificial delay
                  CALL f_sleep(sleep_seconds)
               ENDIF
               call random_number(pot)
               pot = pot - 0.5                
               call random_number(forces)
               forces = forces - 0.5
               call random_number(virial)
               virial = virial - 0.5
               call random_number(dip)
               dip = dip - 0.5
            ELSEIF (vstyle == 1) THEN ! q-AQUA-pol potential
               box = 0.d0 
               box(1) = cell_h(1,1)
               box(2) = cell_h(2,2)
               box(3) = cell_h(3,3)
               call fg_all(atoms,pot,forces,virial,box)
            ENDIF
            hasdata = .true. ! Signal that we have data ready to be passed back to the wrapper
         ELSEIF (trim(header) == "GETFORCE") THEN  ! The driver calculation is finished, it's time to send the results back to the wrapper

            ! Data must be re-formatted (and units converted) in the units and shapes used in the wrapper
            DO i = 1, nat
               msgbuffer(3*(i-1)+1:3*i) = forces(i,:)
            ENDDO
            virial = transpose(virial)

            CALL writebuffer(socket,"FORCEREADY  ",MSGLEN)
            IF (verbose > 1) WRITE(*,*) "    !write!=> ", "FORCEREADY  "
            CALL writebuffer(socket,pot)  ! Writing the potential
            IF (verbose > 1) WRITE(*,*) "    !write!=> pot: ", pot
            CALL writebuffer(socket,nat)  ! Writing the number of atoms
            IF (verbose > 1) WRITE(*,*) "    !write!=> nat:", nat
            CALL writebuffer(socket,msgbuffer,3*nat) ! Writing the forces
            IF (verbose > 1) WRITE(*,*) "    !write!=> forces:", msgbuffer
            CALL writebuffer(socket,reshape(virial,(/9/)),9)  ! Writing the virial tensor, NOT divided by the volume
            IF (verbose > 1) WRITE(*,*) "    !write!=> strss: ", reshape(virial,(/9/))

 125  format(es21.14,a,es21.14,a,es21.14,a,es21.14,a,es21.14,a,es21.14,a)
 126  format(es21.14,a,es21.14,a,es21.14,a,es21.14,a,es21.14,a)

            cbuf = 1 ! Size of the "extras" string
            CALL writebuffer(socket,cbuf) ! This would write out the "extras" string, but in this case we only use a dummy string.
            IF (verbose > 1) WRITE(*,*)  &
     &         "    !write!=> extra_length: ", cbuf
            CALL writebuffer(socket,' ',1)
            IF (verbose > 1) WRITE(*,*)  &
     &         "    !write!=> extra: empty"
            hasdata = .false.
         ELSEIF (trim(header) == "EXIT") THEN
            EXIT
         ELSE
            WRITE(*,*) " Unexpected header ", header
            STOP "ENDED"
         ENDIF
      ENDDO
      IF (nat > 0) DEALLOCATE(atoms, forces, msgbuffer, friction)
      STOP
 
    CONTAINS
      SUBROUTINE helpmessage
         ! Help banner
         WRITE(*,*) " SYNTAX: driver.x [-u] -a address -p port -m [dummy|q-aqua-pol] "
         WRITE(*,*) "         -o 'comma_separated_parameters' [-S sockets_prefix] [-v] "
         WRITE(*,*) ""
         WRITE(*,*) " For dummy, use the optional -o sleep_seconds to add a delay"
         WRITE(*,*) " For q-AQUA-pol, no options are needed "
       END SUBROUTINE helpmessage

   END PROGRAM
