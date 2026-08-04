MODULE px
!..use and access
use inv, wp=>inv_wp
use pes
implicit none
private
public :: &
  px_lsq, px_vlsq, px_lsqs, px_errf, px_errvf, &
  px_null_nbase, px_null_base
public &
  px_x2_nbase, px_x2_base, &
  px_x1y1_nbase, px_x1y1_base, &
  px_x3_nbase, px_x3_base, &
  px_x2y1_nbase, px_x2y1_base, &
  px_x1y2_nbase, px_x1y2_base, &
  px_x1y1z1_nbase, px_x1y1z1_base, &
  px_x1z2_nbase, px_x1z2_base, &
  px_x4_nbase, px_x4_base, &
  px_x3y1_nbase, px_x3y1_base, &
  px_x2y2_nbase, px_x2y2_base, &
  px_x1y3_nbase, px_x1y3_base, &
  px_x2y1z1_nbase, px_x2y1z1_base, &
  px_x1y2z1_nbase, px_x1y2z1_base, &
  px_x1y1z2_nbase, px_x1y1z2_base, &
  px_x1y1z1u1_nbase, px_x1y1z1u1_base, &
  px_x5_nbase, px_x5_base, &
  px_x4y1_nbase, px_x4y1_base, &
  px_x3y2_nbase, px_x3y2_base, &
  px_x2y3_nbase, px_x2y3_base, &
  px_x3y1z1_nbase, px_x3y1z1_base, &
  px_x2y2z1_nbase, px_x2y2z1_base, &
  px_x1y3z1_nbase, px_x1y3z1_base, &
  px_x2y1z2_nbase, px_x2y1z2_base, &
  px_x2y1z1u1_nbase, px_x2y1z1u1_base, &
  px_x5y1_nbase, px_x5y1_base, &
  px_x4y2_nbase, px_x4y2_base, &
  px_x3y3_nbase, px_x3y3_base, &
  px_x4y1z1_nbase, px_x4y1z1_base, &
  px_x3y2z1_nbase, px_x3y2z1_base, &
  px_x2y3z1_nbase, px_x2y3z1_base, &
  px_x2y2z2_nbase, px_x2y2z2_base, &
  px_x3y1z1u1_nbase, px_x3y1z1u1_base, &
  px_x2y2z1u1_nbase, px_x2y2z1u1_base, &
  px_x5y2_nbase, px_x5y2_base, &
  px_x4y3_nbase, px_x4y3_base, &
  px_x5y1z1_nbase, px_x5y1z1_base, &
  px_x4y2z1_nbase, px_x4y2z1_base, &
  px_x3y3z1_nbase, px_x3y3z1_base, &
  px_x3y2z2_nbase, px_x3y2z2_base, &
  px_x4y1z1u1_nbase, px_x4y1z1u1_base, &
  px_x3y2z1u1_nbase, px_x3y2z1u1_base, &
  px_x6y2_nbase, px_x6y2_base, &
  px_x5y3_nbase, px_x5y3_base, &
  px_x4y4_nbase, px_x4y4_base, &
  px_x5y2z1_nbase, px_x5y2z1_base, &
  px_x4y3z1_nbase, px_x4y3z1_base, &
  px_x4y2z2_nbase, px_x4y2z2_base, &
  px_x3y3z2_nbase, px_x3y3z2_base, &
  px_x5y1z1u1_nbase, px_x5y1z1u1_base, &
  px_x4y2z1u1_nbase, px_x4y2z1u1_base, &
  px_x6y3_nbase, px_x6y3_base, &
  px_x5y4_nbase, px_x5y4_base, &
  px_x5y3z1_nbase, px_x5y3z1_base, &
  px_x4y4z1_nbase, px_x4y4z1_base, &
  px_x3y3z3_nbase, px_x3y3z3_base, &
  px_x7y3_nbase, px_x7y3_base, &
  px_x6y3z1_nbase, px_x6y3z1_base, &
  px_x4y4z2_nbase, px_x4y4z2_base
!  px_x9y4_nbase, px_x9y4_base
public &
  px_x2_nvbase, px_x2_vbase, &
  px_x1y1_nvbase, px_x1y1_vbase, &
  px_x3_nvbase, px_x3_vbase, &
  px_x2y1_nvbase, px_x2y1_vbase, &
  px_x1y2_nvbase, px_x1y2_vbase, &
  px_x1y1z1_nvbase, px_x1y1z1_vbase, &
  px_x1z2_nvbase, px_x1z2_vbase, &
  px_x4_nvbase, px_x4_vbase, &
  px_x3y1_nvbase, px_x3y1_vbase, &
  px_x2y2_nvbase, px_x2y2_vbase, &
  px_x1y3_nvbase, px_x1y3_vbase, &
  px_x2y1z1_nvbase, px_x2y1z1_vbase, &
  px_x1y2z1_nvbase, px_x1y2z1_vbase, &
  px_x1y1z2_nvbase, px_x1y1z2_vbase, &
  px_x1y1z1u1_nvbase, px_x1y1z1u1_vbase, &
  px_x5_nvbase, px_x5_vbase, &
  px_x4y1_nvbase, px_x4y1_vbase, &
  px_x3y2_nvbase, px_x3y2_vbase, &
  px_x2y3_nvbase, px_x2y3_vbase, &
  px_x3y1z1_nvbase, px_x3y1z1_vbase, &
  px_x2y2z1_nvbase, px_x2y2z1_vbase, &
  px_x1y3z1_nvbase, px_x1y3z1_vbase, &
  px_x2y1z2_nvbase, px_x2y1z2_vbase, &
  px_x2y1z1u1_nvbase, px_x2y1z1u1_vbase, &
  px_x5y1_nvbase, px_x5y1_vbase, &
  px_x4y2_nvbase, px_x4y2_vbase, &
  px_x3y3_nvbase, px_x3y3_vbase, &
  px_x4y1z1_nvbase, px_x4y1z1_vbase, &
  px_x3y2z1_nvbase, px_x3y2z1_vbase, &
  px_x2y3z1_nvbase, px_x2y3z1_vbase, &
  px_x2y2z2_nvbase, px_x2y2z2_vbase, &
  px_x3y1z1u1_nvbase, px_x3y1z1u1_vbase, &
  px_x2y2z1u1_nvbase, px_x2y2z1u1_vbase, &
  px_x5y2_nvbase, px_x5y2_vbase, &
  px_x4y3_nvbase, px_x4y3_vbase, &
  px_x5y1z1_nvbase, px_x5y1z1_vbase, &
  px_x4y2z1_nvbase, px_x4y2z1_vbase, &
  px_x3y3z1_nvbase, px_x3y3z1_vbase, &
  px_x3y2z2_nvbase, px_x3y2z2_vbase, &
  px_x4y1z1u1_nvbase, px_x4y1z1u1_vbase, &
  px_x3y2z1u1_nvbase, px_x3y2z1u1_vbase, &
  px_x6y2_nvbase, px_x6y2_vbase, &
  px_x5y3_nvbase, px_x5y3_vbase, &
  px_x4y4_nvbase, px_x4y4_vbase, &
  px_x5y2z1_nvbase, px_x5y2z1_vbase, &
  px_x4y3z1_nvbase, px_x4y3z1_vbase, &
  px_x4y2z2_nvbase, px_x4y2z2_vbase, &
  px_x3y3z2_nvbase, px_x3y3z2_vbase, &
  px_x5y1z1u1_nvbase, px_x5y1z1u1_vbase, &
  px_x4y2z1u1_nvbase, px_x4y2z1u1_vbase, &
  px_x6y3_nvbase, px_x6y3_vbase, &
  px_x5y4_nvbase, px_x5y4_vbase, &
  px_x5y3z1_nvbase, px_x5y3z1_vbase, &
  px_x4y4z1_nvbase, px_x4y4z1_vbase, &
  px_x3y3z3_nvbase, px_x3y3z3_vbase, &
  px_x7y3_nvbase, px_x7y3_vbase, &
  px_x6y3z1_nvbase, px_x6y3z1_vbase, &
  px_x4y4z2_nvbase, px_x4y4z2_vbase
!  px_x9y4_nvbase, px_x9y4_vbase
!..data
save
character (len=*), parameter, public :: &
  pes_x2_sysold_dflt='x1', &
  pes_x2_sysnew_dflt='2', &
  pes_x1y1_sysold_dflt='x1 y1', &
  pes_x1y1_sysnew_dflt='11', &
  pes_x3_sysold_dflt='x1', &
  pes_x3_sysnew_dflt='2 3', &
  pes_x2y1_sysold_dflt='x1 y1', &
  pes_x2y1_sysnew_dflt='2 11 21', &
  pes_x1y2_sysold_dflt='x1 y1', &
  pes_x1y2_sysnew_dflt='11 02 12', &
  pes_x1y1z1_sysold_dflt='x1 y1 z1', &
  pes_x1y1z1_sysnew_dflt='11 101 011 111', &
  pes_x1z2_sysold_dflt='x1 z1', &
  pes_x1z2_sysnew_dflt='11 02 12', &
  pes_y1z2_sysold_dflt='y1 z1', &
  pes_y1z2_sysnew_dflt='11 02 12', &
  pes_x4_sysold_dflt='x1', &
  pes_x4_sysnew_dflt='2 3 4', &
  pes_x3y1_sysold_dflt='x1 y1', &
  pes_x3y1_sysnew_dflt='2 11 3 21 31', &
  pes_x2y2_sysold_dflt='x1 y1', &
  pes_x2y2_sysnew_dflt='2 11 02 21 12 22', &
  pes_x1y3_sysold_dflt='x1 y1', &
  pes_x1y3_sysnew_dflt='11 02 12 03 13', &
  pes_x2y1z1_sysold_dflt='x1 y1 z1', &
  pes_x2y1z1_sysnew_dflt='2 11 101 011 21 201 111 211', &
  pes_x1y2z1_sysold_dflt='x1 y1 z1', &
  pes_x1y2z1_sysnew_dflt='11 02 101 011 12 111 021 121', &
  pes_x1y1z2_sysold_dflt='x1 y1 z1', &
  pes_x1y1z2_sysnew_dflt='11 101 011 002 111 102 012 112', &
  pes_x1y1z1u1_sysold_dflt='x1 y1 z1 u1', &
  pes_x1y1z1u1_sysnew_dflt='11 101 011 1001 0101 0011 '// &
    '111 1101 1011 0111 1111', &
  pes_x5_sysold_dflt='x1', &
  pes_x5_sysnew_dflt='2 3 4 5', &
  pes_x4y1_sysold_dflt='x1 y1', &
  pes_x4y1_sysnew_dflt='2 11 3 21 4 31 41', &
  pes_x2y3_sysold_dflt='x1 y1', &
  pes_x2y3_sysnew_dflt='2 11 02 21 12 03 22 13 23', &
  pes_x3y2_sysold_dflt='x1 y1', &
  pes_x3y2_sysnew_dflt='2 11 02 3 21 12 31 22 32', &
  pes_x3y1z1_sysold_dflt='x1 y1 z1', &
  pes_x3y1z1_sysnew_dflt='2 11 101 011 3 21 201 111 31 301 211 311', &
  pes_x2y2z1_sysold_dflt='x1 y1 z1', &
  pes_x2y2z1_sysnew_dflt='2 11 02 101 011 21 12 201 111 021 22 211 121 221', &
  pes_x1y3z1_sysold_dflt='x1 y1 z1', &
  pes_x1y3z1_sysnew_dflt='11 02 101 011 12 03 111 021 13 121 031 131', &
  pes_x2y1z2_sysold_dflt='x1 y1 z1', &
  pes_x2y1z2_sysnew_dflt='2 11 101 011 002 21 201 111 102 012 '// &
    '211 202 112 212', &
  pes_x2y1z1u1_sysold_dflt='x1 y1 z1 u1', &
  pes_x2y1z1u1_sysnew_dflt='2 11 101 011 1001 0101 0011 '// &
    '21 201 111 2001 1101 1011 0111 211 2101 2011 1111 2111', &
  pes_x5y1_sysold_dflt='x1 y1', &
  pes_x5y1_sysnew_dflt='2 11 3 21 4 31 5 41 51', &
  pes_x4y2_sysold_dflt='x1 y1', &
  pes_x4y2_sysnew_dflt='2 11 02 3 21 12 4 31 22 41 32 42', &
  pes_x3y3_sysold_dflt='x1 y1', &
  pes_x3y3_sysnew_dflt='2 11 02 3 21 12 03 31 22 13 32 23 33', &
  pes_x4y1z1_sysold_dflt='x1 y1 z1', &
  pes_x4y1z1_sysnew_dflt='2 11 101 011 3 21 201 111 4 31 301 211 '// &
    '41 401 311 411', &
  pes_x3y2z1_sysold_dflt='x1 y1 z1', &
  pes_x3y2z1_sysnew_dflt='2 11 02 101 011 3 21 12 201 111 021 '// &
    '31 22 301 211 121 32 311 221 321', &
  pes_x2y3z1_sysold_dflt='x1 y1 z1', &
  pes_x2y3z1_sysnew_dflt='2 11 02 101 011 21 12 03 201 111 021 '// &
    '22 13 211 121 031 23 221 131 231', &
  pes_x2y2z2_sysold_dflt='x1 y1 z1', &
  pes_x2y2z2_sysnew_dflt='2 11 02 101 011 002 '// &
    '21 12 201 111 021 102 012 '// &
    '22 211 121 202 112 022 221 212 122 222', &
  pes_x3y1z1u1_sysold_dflt='x1 y1 z1 u1', &
  pes_x3y1z1u1_sysnew_dflt='2 11 101 011 1001 0101 0011 '// &
    '3 21 201 111 2001 1101 1011 0111 31 301 211 3001 2101 2011 1111 '// &
    '311 3101 3011 2111 3111', &
  pes_x2y2z1u1_sysold_dflt='x1 y1 z1 u1', &
  pes_x2y2z1u1_sysnew_dflt='2 11 02 101 011 1001 0101 0011 '// &
    '21 12 201 111 021 2001 1101 0201 1011 0111 '// &
    '22 211 121 2101 1201 2011 1111 0211 '// &
    '221 2201 1211 2111 2211', &
  pes_x5y2_sysold_dflt='x1 y1', &
  pes_x5y2_sysnew_dflt='2 11 02 3 21 12 4 31 22 5 41 32 51 42 52', &
  pes_x4y3_sysold_dflt='x1 y1', &
  pes_x4y3_sysnew_dflt='2 11 02 3 21 12 03 4 31 22 13 41 32 23 42 33 43', &
  pes_x5y1z1_sysold_dflt='x1 y1 z1', &
  pes_x5y1z1_sysnew_dflt='2 11 101 011 3 21 201 111 4 31 301 211 '// &
    '5 41 401 311 51 501 411 511', &
  pes_x4y2z1_sysold_dflt='x1 y1 z1', &
  pes_x4y2z1_sysnew_dflt='2 11 02 101 011 3 21 12 201 111 021 '// &
    '4 31 22 301 211 121 41 32 401 311 221 42 411 321 421', &
  pes_x3y3z1_sysold_dflt='x1 y1 z1', &
  pes_x3y3z1_sysnew_dflt='2 11 02 101 011 3 21 12 03 201 111 021 '// &
    '31 22 13 301 211 121 031 32 23 311 221 131 33 321 231 331', &
  pes_x3y2z2_sysold_dflt='x1 y1 z1', &
  pes_x3y2z2_sysnew_dflt='2 11 02 101 011 002 '// &
    '3 21 12 201 111 021 102 012 '// &
    '31 22 301 211 121 202 112 022 '// &
    '32 311 221 302 212 122 '// &
    '321 312 222 322', &
  pes_x4y1z1u1_sysold_dflt='x1 y1 z1 u1', &
  pes_x4y1z1u1_sysnew_dflt='2 11 101 011 1001 0101 0011 '// &
    '3 21 201 111 2001 1101 1011 0111 '// &
    '4 31 301 211 3001 2101 2011 1111 '// &
    '41 401 311 4001 3101 3011 2111 '// &
    '411 4101 4011 3111 '// &
    '4111', &
  pes_x3y2z1u1_sysold_dflt='x1 y1 z1 u1', &
  pes_x3y2z1u1_sysnew_dflt='2 11 02 101 011 1001 0101 0011 '// &
    '3 21 12 201 111 021 2001 1101 0201 1011 0111 '// &
    '31 22 301 211 121 3001 2101 1201 2011 1111 0211 '// &
    '32 311 221 3101 2201 3011 2111 1211 '// &
    '321 3201 3111 2211 '// &
    '3211', &
  pes_x6y2_sysold_dflt='x1 y1', &
  pes_x6y2_sysnew_dflt='2 11 02 3 21 12 4 31 22 5 41 32 6 51 42 '// &
    '61 52 62', &
  pes_x5y3_sysold_dflt='x1 y1', &
  pes_x5y3_sysnew_dflt='2 11 02 3 21 12 03 '// &
    '4 31 22 13 5 41 32 23 51 42 33 52 43 53', &
  pes_x4y4_sysold_dflt='x1 y1', &
  pes_x4y4_sysnew_dflt='2 11 02 44', &
  pes_x5y2z1_sysold_dflt='x1 y1 z1', &
  pes_x5y2z1_sysnew_dflt='2 11 02 101 011 3 21 12 201 111 021 '// &
    '4 31 22 301 211 121 5 41 32 401 311 221 '// &
    '51 42 501 411 321 52 511 421 521', &
  pes_x4y3z1_sysold_dflt='x1 y1 z1', &
  pes_x4y3z1_sysnew_dflt='2 11 02 101 011 3 21 12 03 201 111 021 '// &
    '4 31 22 13 301 211 121 031 41 32 23 401 311 221 131 '// &
    '42 33 411 321 231 43 421 331 431', &
  pes_x4y2z2_sysold_dflt='x1 y1 z1', &
  pes_x4y2z2_sysnew_dflt='2 11 02 101 011 002 '// &
    '3 21 12 201 111 021 102 012 '// &
    '4 31 22 301 211 121 202 112 022 '// &
    '41 32 401 311 221 302 212 122 '// &
    '42 411 321 402 312 222 '// &
    '421 412 322 422', &
  pes_x3y3z2_sysold_dflt='x1 y1 z1', &
  pes_x3y3z2_sysnew_dflt='2 11 02 101 011 002 '// &
    '3 21 12 03 201 111 021 102 012 '// &
    '31 22 13 301 211 121 031 202 112 022 '// &
    '32 23 311 221 131 302 212 122 032 '// &
    '33 321 231 312 222 132 '// &
    '331 322 232 332', &
  pes_x5y1z1u1_sysold_dflt='x1 y1 z1 u1', &
  pes_x5y1z1u1_sysnew_dflt='2 11 101 011 1001 0101 0011 '// &
    '3 21 201 111 2001 1101 1011 0111 '// &
    '4 31 301 211 3001 2101 2011 1111 '// &
    '5 41 401 311 4001 3101 3011 2111 '// &
    '51 501 411 5001 4101 4011 3111 '// &
    '511 5101 5011 4111 '// &
    '5111', &
  pes_x4y2z1u1_sysold_dflt='x1 y1 z1 u1', &
  pes_x4y2z1u1_sysnew_dflt='2 11 02 101 011 1001 0101 0011 '// &
    '3 21 12 201 111 021 2001 1101 0201 1011 0111 '// &
    '4 31 22 301 211 121 3001 2101 1201 2011 1111 0211 '// &
    '41 32 401 311 221 4001 3101 2201 3011 2111 1211 '// &
    '42 411 321 4101 3201 4011 3111 2211 '// &
    '421 4201 4111 3211 '// &
    '4211', &
  pes_x6y3_sysold_dflt='x1 y1', &
  pes_x6y3_sysnew_dflt='2 11 02 3 21 12 03 4 31 22 13 5 41 32 23 '// &
    '6 51 42 33 61 52 43 62 53 63', &
  pes_x5y4_sysold_dflt='x1 y1', &
  pes_x5y4_sysnew_dflt='2 11 02 54', &
  pes_x5y3z1_sysold_dflt='x1 y1 z1', &
  pes_x5y3z1_sysnew_dflt='2 11 02 101 011 531', &
  pes_x4y4z1_sysold_dflt='x1 y1 z1', &
  pes_x4y4z1_sysnew_dflt='2 11 02 101 011 441', &
  pes_x3y3z3_sysold_dflt='x1 y1 z1', &
  pes_x3y3z3_sysnew_dflt='2 11 02 101 011 002 '// &
    '3 21 12 03 201 111 021 102 012 003 '// &
    '31 22 13 301 211 121 031 202 112 022 103 013 '// &
    '32 23 311 221 131 302 212 122 032 203 113 023 '// &
    '33 321 231 312 222 132 303 213 123 033 '// &
    '331 322 232 313 223 133 332 323 233 333', &
  pes_x7y3_sysold_dflt='x1 y1', &
  pes_x7y3_sysnew_dflt='2 11 02 3 21 12 03 4 31 22 13 5 41 32 23 '// &
    '6 51 42 33 7 61 52 43 71 62 53 72 63 73', &
  pes_x6y3z1_sysold_dflt='x1 y1 z1', &
  pes_x6y3z1_sysnew_dflt='2 11 02 101 011 631', &
  pes_x4y4z2_sysold_dflt='x1 y1 z1', &
  pes_x4y4z2_sysnew_dflt='2 11 02 101 011 002 442'
!  pes_x9y4_sysold_dflt='x1 y1', &
!  pes_x9y4_sysnew_dflt=pes_x5y2_sysnew_dflt
character (len=240), public :: &
  pes_x2_sysold=pes_x2_sysold_dflt, &
  pes_x2_sysnew=pes_x2_sysnew_dflt, &
  pes_x1y1_sysold=pes_x1y1_sysold_dflt, &
  pes_x1y1_sysnew=pes_x1y1_sysnew_dflt, &
  pes_x3_sysold=pes_x3_sysold_dflt, &
  pes_x3_sysnew=pes_x3_sysnew_dflt, &
  pes_x2y1_sysold=pes_x2y1_sysold_dflt, &
  pes_x2y1_sysnew=pes_x2y1_sysnew_dflt, &
  pes_x1y2_sysold=pes_x1y2_sysold_dflt, &
  pes_x1y2_sysnew=pes_x1y2_sysnew_dflt, &
  pes_x1y1z1_sysold=pes_x1y1z1_sysold_dflt, &
  pes_x1y1z1_sysnew=pes_x1y1z1_sysnew_dflt, &
  pes_x1z2_sysold=pes_x1z2_sysold_dflt, &
  pes_x1z2_sysnew=pes_x1z2_sysnew_dflt, &
  pes_y1z2_sysold=pes_y1z2_sysold_dflt, &
  pes_y1z2_sysnew=pes_y1z2_sysnew_dflt, &
  pes_x4_sysold=pes_x4_sysold_dflt, &
  pes_x4_sysnew=pes_x4_sysnew_dflt, &
  pes_x3y1_sysold=pes_x3y1_sysold_dflt, &
  pes_x3y1_sysnew=pes_x3y1_sysnew_dflt, &
  pes_x2y2_sysold=pes_x2y2_sysold_dflt, &
  pes_x2y2_sysnew=pes_x2y2_sysnew_dflt, &
  pes_x1y3_sysold=pes_x1y3_sysold_dflt, &
  pes_x1y3_sysnew=pes_x1y3_sysnew_dflt, &
  pes_x2y1z1_sysold=pes_x2y1z1_sysold_dflt, &
  pes_x2y1z1_sysnew=pes_x2y1z1_sysnew_dflt, &
  pes_x1y2z1_sysold=pes_x1y2z1_sysold_dflt, &
  pes_x1y2z1_sysnew=pes_x1y2z1_sysnew_dflt, &
  pes_x1y1z2_sysold=pes_x1y1z2_sysold_dflt, &
  pes_x1y1z2_sysnew=pes_x1y1z2_sysnew_dflt, &
  pes_x1y1z1u1_sysold=pes_x1y1z1u1_sysold_dflt, &
  pes_x1y1z1u1_sysnew=pes_x1y1z1u1_sysnew_dflt, &
  pes_x5_sysold=pes_x5_sysold_dflt, &
  pes_x5_sysnew=pes_x5_sysnew_dflt, &
  pes_x4y1_sysold=pes_x4y1_sysold_dflt, &
  pes_x4y1_sysnew=pes_x4y1_sysnew_dflt, &
  pes_x3y2_sysold=pes_x3y2_sysold_dflt, &
  pes_x3y2_sysnew=pes_x3y2_sysnew_dflt, &
  pes_x2y3_sysold=pes_x2y3_sysold_dflt, &
  pes_x2y3_sysnew=pes_x2y3_sysnew_dflt, &
  pes_x3y1z1_sysold=pes_x3y1z1_sysold_dflt, &
  pes_x3y1z1_sysnew=pes_x3y1z1_sysnew_dflt, &
  pes_x2y2z1_sysold=pes_x2y2z1_sysold_dflt, &
  pes_x2y2z1_sysnew=pes_x2y2z1_sysnew_dflt, &
  pes_x1y3z1_sysold=pes_x1y3z1_sysold_dflt, &
  pes_x1y3z1_sysnew=pes_x1y3z1_sysnew_dflt, &
  pes_x2y1z2_sysold=pes_x2y1z2_sysold_dflt, &
  pes_x2y1z2_sysnew=pes_x2y1z2_sysnew_dflt, &
  pes_x2y1z1u1_sysold=pes_x2y1z1u1_sysold_dflt, &
  pes_x2y1z1u1_sysnew=pes_x2y1z1u1_sysnew_dflt, &
  pes_x5y1_sysold=pes_x5y1_sysold_dflt, &
  pes_x5y1_sysnew=pes_x5y1_sysnew_dflt, &
  pes_x4y2_sysold=pes_x4y2_sysold_dflt, &
  pes_x4y2_sysnew=pes_x4y2_sysnew_dflt, &
  pes_x3y3_sysold=pes_x3y3_sysold_dflt, &
  pes_x3y3_sysnew=pes_x3y3_sysnew_dflt, &
  pes_x4y1z1_sysold=pes_x4y1z1_sysold_dflt, &
  pes_x4y1z1_sysnew=pes_x4y1z1_sysnew_dflt, &
  pes_x3y2z1_sysold=pes_x3y2z1_sysold_dflt, &
  pes_x3y2z1_sysnew=pes_x3y2z1_sysnew_dflt, &
  pes_x2y3z1_sysold=pes_x2y3z1_sysold_dflt, &
  pes_x2y3z1_sysnew=pes_x2y3z1_sysnew_dflt, &
  pes_x2y2z2_sysold=pes_x2y2z2_sysold_dflt, &
  pes_x2y2z2_sysnew=pes_x2y2z2_sysnew_dflt, &
  pes_x3y1z1u1_sysold=pes_x3y1z1u1_sysold_dflt, &
  pes_x3y1z1u1_sysnew=pes_x3y1z1u1_sysnew_dflt, &
  pes_x2y2z1u1_sysold=pes_x2y2z1u1_sysold_dflt, &
  pes_x2y2z1u1_sysnew=pes_x2y2z1u1_sysnew_dflt, &
  pes_x5y2_sysold=pes_x5y2_sysold_dflt, &
  pes_x5y2_sysnew=pes_x5y2_sysnew_dflt, &
  pes_x4y3_sysold=pes_x4y3_sysold_dflt, &
  pes_x4y3_sysnew=pes_x4y3_sysnew_dflt, &
  pes_x5y1z1_sysold=pes_x5y1z1_sysold_dflt, &
  pes_x5y1z1_sysnew=pes_x5y1z1_sysnew_dflt, &
  pes_x4y2z1_sysold=pes_x4y2z1_sysold_dflt, &
  pes_x4y2z1_sysnew=pes_x4y2z1_sysnew_dflt, &
  pes_x3y3z1_sysold=pes_x3y3z1_sysold_dflt, &
  pes_x3y3z1_sysnew=pes_x3y3z1_sysnew_dflt, &
  pes_x3y2z2_sysold=pes_x3y2z2_sysold_dflt, &
  pes_x3y2z2_sysnew=pes_x3y2z2_sysnew_dflt, &
  pes_x4y1z1u1_sysold=pes_x4y1z1u1_sysold_dflt, &
  pes_x4y1z1u1_sysnew=pes_x4y1z1u1_sysnew_dflt, &
  pes_x3y2z1u1_sysold=pes_x3y2z1u1_sysold_dflt, &
  pes_x3y2z1u1_sysnew=pes_x3y2z1u1_sysnew_dflt, &
  pes_x6y2_sysold=pes_x6y2_sysold_dflt, &
  pes_x6y2_sysnew=pes_x6y2_sysnew_dflt, &
  pes_x5y3_sysold=pes_x5y3_sysold_dflt, &
  pes_x5y3_sysnew=pes_x5y3_sysnew_dflt, &
  pes_x4y4_sysold=pes_x4y4_sysold_dflt, &
  pes_x4y4_sysnew=pes_x4y4_sysnew_dflt, &
  pes_x5y2z1_sysold=pes_x5y2z1_sysold_dflt, &
  pes_x5y2z1_sysnew=pes_x5y2z1_sysnew_dflt, &
  pes_x4y3z1_sysold=pes_x4y3z1_sysold_dflt, &
  pes_x4y3z1_sysnew=pes_x4y3z1_sysnew_dflt, &
  pes_x4y2z2_sysold=pes_x4y2z2_sysold_dflt, &
  pes_x4y2z2_sysnew=pes_x4y2z2_sysnew_dflt, &
  pes_x3y3z2_sysold=pes_x3y3z2_sysold_dflt, &
  pes_x3y3z2_sysnew=pes_x3y3z2_sysnew_dflt, &
  pes_x5y1z1u1_sysold=pes_x5y1z1u1_sysold_dflt, &
  pes_x5y1z1u1_sysnew=pes_x5y1z1u1_sysnew_dflt, &
  pes_x4y2z1u1_sysold=pes_x4y2z1u1_sysold_dflt, &
  pes_x4y2z1u1_sysnew=pes_x4y2z1u1_sysnew_dflt, &
  pes_x6y3_sysold=pes_x6y3_sysold_dflt, &
  pes_x6y3_sysnew=pes_x6y3_sysnew_dflt, &
  pes_x5y4_sysold=pes_x5y4_sysold_dflt, &
  pes_x5y4_sysnew=pes_x5y4_sysnew_dflt, &
  pes_x5y3z1_sysold=pes_x5y3z1_sysold_dflt, &
  pes_x5y3z1_sysnew=pes_x5y3z1_sysnew_dflt, &
  pes_x4y4z1_sysold=pes_x4y4z1_sysold_dflt, &
  pes_x4y4z1_sysnew=pes_x4y4z1_sysnew_dflt, &
  pes_x3y3z3_sysold=pes_x3y3z3_sysold_dflt, &
  pes_x3y3z3_sysnew=pes_x3y3z3_sysnew_dflt, &
  pes_x7y3_sysold=pes_x7y3_sysold_dflt, &
  pes_x7y3_sysnew=pes_x7y3_sysnew_dflt, &
  pes_x6y3z1_sysold=pes_x6y3z1_sysold_dflt, &
  pes_x6y3z1_sysnew=pes_x6y3z1_sysnew_dflt, &
  pes_x4y4z2_sysold=pes_x4y4z2_sysold_dflt, &
  pes_x4y4z2_sysnew=pes_x4y4z2_sysnew_dflt
!  pes_x9y4_sysold=pes_x9y4_sysold_dflt, &
!  pes_x9y4_sysnew=pes_x9y4_sysnew_dflt
integer, public :: px_ng=3, px_ng2=3
real (kind=wp), public :: &
  px_rcond=1.0e-11_wp, &
  px_wtgf=0.2_wp*pes_bohr, &
  px_wtg2f=0.05_wp*pes_bohr**2
type (cx_t), public :: px_pcv(2:12)= &
  (/ cx_null, &
  cx_null, &
  cx_null, &
  cx_null, &
  cx_null, &
  cx_null, &
  cx_null, &
  cx_null, &
  cx_null, &
  cx_null, &
  cx_null /)
type (cx_t), public :: px_vpcv(2:12)= &
  (/ cx_null, &
  cx_null, &
  cx_null, &
  cx_null, &
  cx_null, &
  cx_null, &
  cx_null, &
  cx_null, &
  cx_null, &
  cx_null, &
  cx_null /)
!..procedures
CONTAINS
include 'px_lsq.f90'
include 'px_vlsq.f90'
include 'px_lsqs.f90'
include 'px_errf.f90'
include 'px_errvf.f90'
include 'px_null_nbase.f90'
include 'px_null_base.f90'
include 'px_x2_nbase.f90'
include 'px_x2_base.f90'
include 'px_x1y1_nbase.f90'
include 'px_x1y1_base.f90'
include 'px_x3_nbase.f90'
include 'px_x3_base.f90'
include 'px_x2y1_nbase.f90'
include 'px_x2y1_base.f90'
include 'px_x1y2_nbase.f90'
include 'px_x1y2_base.f90'
include 'px_x1y1z1_nbase.f90'
include 'px_x1y1z1_base.f90'
include 'px_x1z2_nbase.f90'
include 'px_x1z2_base.f90'
include 'px_x4_nbase.f90'
include 'px_x4_base.f90'
include 'px_x3y1_nbase.f90'
include 'px_x3y1_base.f90'
include 'px_x2y2_nbase.f90'
include 'px_x2y2_base.f90'
include 'px_x1y3_nbase.f90'
include 'px_x1y3_base.f90'
include 'px_x2y1z1_nbase.f90'
include 'px_x2y1z1_base.f90'
include 'px_x1y2z1_nbase.f90'
include 'px_x1y2z1_base.f90'
include 'px_x1y1z2_nbase.f90'
include 'px_x1y1z2_base.f90'
include 'px_x1y1z1u1_nbase.f90'
include 'px_x1y1z1u1_base.f90'
include 'px_x5_nbase.f90'
include 'px_x5_base.f90'
include 'px_x4y1_nbase.f90'
include 'px_x4y1_base.f90'
include 'px_x3y2_nbase.f90'
include 'px_x3y2_base.f90'
include 'px_x2y3_nbase.f90'
include 'px_x2y3_base.f90'
include 'px_x3y1z1_nbase.f90'
include 'px_x3y1z1_base.f90'
include 'px_x2y2z1_nbase.f90'
include 'px_x2y2z1_base.f90'
include 'px_x1y3z1_nbase.f90'
include 'px_x1y3z1_base.f90'
include 'px_x2y1z2_nbase.f90'
include 'px_x2y1z2_base.f90'
include 'px_x2y1z1u1_nbase.f90'
include 'px_x2y1z1u1_base.f90'
include 'px_x5y1_nbase.f90'
include 'px_x5y1_base.f90'
include 'px_x4y2_nbase.f90'
include 'px_x4y2_base.f90'
include 'px_x3y3_nbase.f90'
include 'px_x3y3_base.f90'
include 'px_x4y1z1_nbase.f90'
include 'px_x4y1z1_base.f90'
include 'px_x3y2z1_nbase.f90'
include 'px_x3y2z1_base.f90'
include 'px_x2y3z1_nbase.f90'
include 'px_x2y3z1_base.f90'
include 'px_x2y2z2_nbase.f90'
include 'px_x2y2z2_base.f90'
include 'px_x3y1z1u1_nbase.f90'
include 'px_x3y1z1u1_base.f90'
include 'px_x2y2z1u1_nbase.f90'
include 'px_x2y2z1u1_base.f90'
include 'px_x5y2_nbase.f90'
include 'px_x5y2_base.f90'
include 'px_x4y3_nbase.f90'
include 'px_x4y3_base.f90'
include 'px_x5y1z1_nbase.f90'
include 'px_x5y1z1_base.f90'
include 'px_x4y2z1_nbase.f90'
include 'px_x4y2z1_base.f90'
include 'px_x3y3z1_nbase.f90'
include 'px_x3y3z1_base.f90'
include 'px_x3y2z2_nbase.f90'
include 'px_x3y2z2_base.f90'
include 'px_x4y1z1u1_nbase.f90'
include 'px_x4y1z1u1_base.f90'
include 'px_x3y2z1u1_nbase.f90'
include 'px_x3y2z1u1_base.f90'
include 'px_x6y2_nbase.f90'
include 'px_x6y2_base.f90'
include 'px_x5y3_nbase.f90'
include 'px_x5y3_base.f90'
include 'px_x4y4_nbase.f90'
include 'px_x4y4_base.f90'
include 'px_x5y2z1_nbase.f90'
include 'px_x5y2z1_base.f90'
include 'px_x4y3z1_nbase.f90'
include 'px_x4y3z1_base.f90'
include 'px_x4y2z2_nbase.f90'
include 'px_x4y2z2_base.f90'
include 'px_x3y3z2_nbase.f90'
include 'px_x3y3z2_base.f90'
include 'px_x5y1z1u1_nbase.f90'
include 'px_x5y1z1u1_base.f90'
include 'px_x4y2z1u1_nbase.f90'
include 'px_x4y2z1u1_base.f90'
include 'px_x6y3_nbase.f90'
include 'px_x6y3_base.f90'
include 'px_x5y4_nbase.f90'
include 'px_x5y4_base.f90'
include 'px_x5y3z1_nbase.f90'
include 'px_x5y3z1_base.f90'
include 'px_x4y4z1_nbase.f90'
include 'px_x4y4z1_base.f90'
include 'px_x3y3z3_nbase.f90'
include 'px_x3y3z3_base.f90'
include 'px_x7y3_nbase.f90'
include 'px_x7y3_base.f90'
include 'px_x6y3z1_nbase.f90'
include 'px_x6y3z1_base.f90'
include 'px_x4y4z2_nbase.f90'
include 'px_x4y4z2_base.f90'
!include 'px_x9y4_nbase.f90'
!include 'px_x9y4_base.f90'
include 'px_x2_nvbase.f90'
include 'px_x2_vbase.f90'
include 'px_x1y1_nvbase.f90'
include 'px_x1y1_vbase.f90'
include 'px_x3_nvbase.f90'
include 'px_x3_vbase.f90'
include 'px_x2y1_nvbase.f90'
include 'px_x2y1_vbase.f90'
include 'px_x1y2_nvbase.f90'
include 'px_x1y2_vbase.f90'
include 'px_x1y1z1_nvbase.f90'
include 'px_x1y1z1_vbase.f90'
include 'px_x1z2_nvbase.f90'
include 'px_x1z2_vbase.f90'
include 'px_x4_nvbase.f90'
include 'px_x4_vbase.f90'
include 'px_x3y1_nvbase.f90'
include 'px_x3y1_vbase.f90'
include 'px_x2y2_nvbase.f90'
include 'px_x2y2_vbase.f90'
include 'px_x1y3_nvbase.f90'
include 'px_x1y3_vbase.f90'
include 'px_x2y1z1_nvbase.f90'
include 'px_x2y1z1_vbase.f90'
include 'px_x1y2z1_nvbase.f90'
include 'px_x1y2z1_vbase.f90'
include 'px_x1y1z2_nvbase.f90'
include 'px_x1y1z2_vbase.f90'
include 'px_x1y1z1u1_nvbase.f90'
include 'px_x1y1z1u1_vbase.f90'
include 'px_x5_nvbase.f90'
include 'px_x5_vbase.f90'
include 'px_x4y1_nvbase.f90'
include 'px_x4y1_vbase.f90'
include 'px_x3y2_nvbase.f90'
include 'px_x3y2_vbase.f90'
include 'px_x2y3_nvbase.f90'
include 'px_x2y3_vbase.f90'
include 'px_x3y1z1_nvbase.f90'
include 'px_x3y1z1_vbase.f90'
include 'px_x2y2z1_nvbase.f90'
include 'px_x2y2z1_vbase.f90'
include 'px_x1y3z1_nvbase.f90'
include 'px_x1y3z1_vbase.f90'
include 'px_x2y1z2_nvbase.f90'
include 'px_x2y1z2_vbase.f90'
include 'px_x2y1z1u1_nvbase.f90'
include 'px_x2y1z1u1_vbase.f90'
include 'px_x5y1_nvbase.f90'
include 'px_x5y1_vbase.f90'
include 'px_x4y2_nvbase.f90'
include 'px_x4y2_vbase.f90'
include 'px_x3y3_nvbase.f90'
include 'px_x3y3_vbase.f90'
include 'px_x4y1z1_nvbase.f90'
include 'px_x4y1z1_vbase.f90'
include 'px_x3y2z1_nvbase.f90'
include 'px_x3y2z1_vbase.f90'
include 'px_x2y3z1_nvbase.f90'
include 'px_x2y3z1_vbase.f90'
include 'px_x2y2z2_nvbase.f90'
include 'px_x2y2z2_vbase.f90'
include 'px_x3y1z1u1_nvbase.f90'
include 'px_x3y1z1u1_vbase.f90'
include 'px_x2y2z1u1_nvbase.f90'
include 'px_x2y2z1u1_vbase.f90'
include 'px_x5y2_nvbase.f90'
include 'px_x5y2_vbase.f90'
include 'px_x4y3_nvbase.f90'
include 'px_x4y3_vbase.f90'
include 'px_x5y1z1_nvbase.f90'
include 'px_x5y1z1_vbase.f90'
include 'px_x4y2z1_nvbase.f90'
include 'px_x4y2z1_vbase.f90'
include 'px_x3y3z1_nvbase.f90'
include 'px_x3y3z1_vbase.f90'
include 'px_x3y2z2_nvbase.f90'
include 'px_x3y2z2_vbase.f90'
include 'px_x4y1z1u1_nvbase.f90'
include 'px_x4y1z1u1_vbase.f90'
include 'px_x3y2z1u1_nvbase.f90'
include 'px_x3y2z1u1_vbase.f90'
include 'px_x6y2_nvbase.f90'
include 'px_x6y2_vbase.f90'
include 'px_x5y3_nvbase.f90'
include 'px_x5y3_vbase.f90'
include 'px_x4y4_nvbase.f90'
include 'px_x4y4_vbase.f90'
include 'px_x5y2z1_nvbase.f90'
include 'px_x5y2z1_vbase.f90'
include 'px_x4y3z1_nvbase.f90'
include 'px_x4y3z1_vbase.f90'
include 'px_x4y2z2_nvbase.f90'
include 'px_x4y2z2_vbase.f90'
include 'px_x3y3z2_nvbase.f90'
include 'px_x3y3z2_vbase.f90'
include 'px_x5y1z1u1_nvbase.f90'
include 'px_x5y1z1u1_vbase.f90'
include 'px_x4y2z1u1_nvbase.f90'
include 'px_x4y2z1u1_vbase.f90'
include 'px_x6y3_nvbase.f90'
include 'px_x6y3_vbase.f90'
include 'px_x5y4_nvbase.f90'
include 'px_x5y4_vbase.f90'
include 'px_x5y3z1_nvbase.f90'
include 'px_x5y3z1_vbase.f90'
include 'px_x4y4z1_nvbase.f90'
include 'px_x4y4z1_vbase.f90'
include 'px_x3y3z3_nvbase.f90'
include 'px_x3y3z3_vbase.f90'
include 'px_x7y3_nvbase.f90'
include 'px_x7y3_vbase.f90'
include 'px_x6y3z1_nvbase.f90'
include 'px_x6y3z1_vbase.f90'
include 'px_x4y4z2_nvbase.f90'
include 'px_x4y4z2_vbase.f90'
!include 'px_x9y4_nvbase.f90'
!include 'px_x9y4_vbase.f90'
END MODULE px
