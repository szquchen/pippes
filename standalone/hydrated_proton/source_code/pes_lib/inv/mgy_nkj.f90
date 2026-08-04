PURE SUBROUTINE mgy_nkj (id, nkj)
integer, intent (in) :: id
integer, intent (out) :: nkj(0:)
!-----------------------------------------------------------------------
nkj = 0
select case (id)
case (0)
case (mg1_id)
 nkj(0:size(mg1_nkj)-1) = mg1_nkj
case (mg2_id)
 nkj(0:size(mg2_nkj)-1) = mg2_nkj
case (mg11_id)
 nkj(0:size(mg11_nkj)-1) = mg11_nkj
case (mg3_id)
 nkj(0:size(mg3_nkj)-1) = mg3_nkj
case (mg21_id)
 nkj(0:size(mg21_nkj)-1) = mg21_nkj
case (mg111_id)
 nkj(0:size(mg111_nkj)-1) = mg111_nkj
case (mg4_id)
 nkj(0:size(mg4_nkj)-1) = mg4_nkj
case (mg31_id)
 nkj(0:size(mg31_nkj)-1) = mg31_nkj
case (mg22_id)
 nkj(0:size(mg22_nkj)-1) = mg22_nkj
case (mg211_id)
 nkj(0:size(mg211_nkj)-1) = mg211_nkj
case (mg1111_id)
 nkj(0:size(mg1111_nkj)-1) = mg1111_nkj
case (mg5_id)
 nkj(0:size(mg5_nkj)-1) = mg5_nkj
case (mg41_id)
 nkj(0:size(mg41_nkj)-1) = mg41_nkj
case (mg32_id)
 nkj(0:size(mg32_nkj)-1) = mg32_nkj
case (mg311_id)
 nkj(0:size(mg311_nkj)-1) = mg311_nkj
case (mg221_id)
 nkj(0:size(mg221_nkj)-1) = mg221_nkj
case (mg2111_id)
 nkj(0:size(mg2111_nkj)-1) = mg2111_nkj
case (mg11111_id)
 nkj(0:size(mg11111_nkj)-1) = mg11111_nkj
case (mg6_id)
 nkj(0:size(mg6_nkj)-1) = mg6_nkj
case (mg51_id)
 nkj(0:size(mg51_nkj)-1) = mg51_nkj
case (mg42_id)
 nkj(0:size(mg42_nkj)-1) = mg42_nkj
case (mg33_id)
 nkj(0:size(mg33_nkj)-1) = mg33_nkj
case (mg411_id)
 nkj(0:size(mg411_nkj)-1) = mg411_nkj
case (mg321_id)
 nkj(0:size(mg321_nkj)-1) = mg321_nkj
case (mg222_id)
 nkj(0:size(mg222_nkj)-1) = mg222_nkj
case (mg3111_id)
 nkj(0:size(mg3111_nkj)-1) = mg3111_nkj
case (mg2211_id)
 nkj(0:size(mg2211_nkj)-1) = mg2211_nkj
case (mg21111_id)
 nkj(0:size(mg21111_nkj)-1) = mg21111_nkj
case (mg111111_id)
 nkj(0:size(mg111111_nkj)-1) = mg111111_nkj
case (mg7_id)
 nkj(0:size(mg7_nkj)-1) = mg7_nkj
case (mg61_id)
 nkj(0:size(mg61_nkj)-1) = mg61_nkj
case (mg52_id)
 nkj(0:size(mg52_nkj)-1) = mg52_nkj
case (mg43_id)
 nkj(0:size(mg43_nkj)-1) = mg43_nkj
case (mg511_id)
 nkj(0:size(mg511_nkj)-1) = mg511_nkj
case (mg421_id)
 nkj(0:size(mg421_nkj)-1) = mg421_nkj
case (mg331_id)
 nkj(0:size(mg331_nkj)-1) = mg331_nkj
case (mg322_id)
 nkj(0:size(mg322_nkj)-1) = mg322_nkj
case (mg4111_id)
 nkj(0:size(mg4111_nkj)-1) = mg4111_nkj
case (mg3211_id)
 nkj(0:size(mg3211_nkj)-1) = mg3211_nkj
case (mg2221_id)
 nkj(0:size(mg2221_nkj)-1) = mg2221_nkj
case (mg31111_id)
 nkj(0:size(mg31111_nkj)-1) = mg31111_nkj
case (mg22111_id)
 nkj(0:size(mg22111_nkj)-1) = mg22111_nkj
case (mg211111_id)
 nkj(0:size(mg211111_nkj)-1) = mg211111_nkj
case (mg1111111_id)
 nkj(0:size(mg1111111_nkj)-1) = mg1111111_nkj
!! case (mg8_id)
!!  nkj(0:size(mg8_nkj)-1) = mg8_nkj
case (mg71_id)
 nkj(0:size(mg71_nkj)-1) = mg71_nkj
case (mg62_id)
 nkj(0:size(mg62_nkj)-1) = mg62_nkj
case (mg53_id)
 nkj(0:size(mg53_nkj)-1) = mg53_nkj
case (mg44_id)
 nkj(0:size(mg44_nkj)-1) = mg44_nkj
case (mg611_id)
 nkj(0:size(mg611_nkj)-1) = mg611_nkj
case (mg521_id)
 nkj(0:size(mg521_nkj)-1) = mg521_nkj
case (mg431_id)
 nkj(0:size(mg431_nkj)-1) = mg431_nkj
case (mg422_id)
 nkj(0:size(mg422_nkj)-1) = mg422_nkj
case (mg332_id)
 nkj(0:size(mg332_nkj)-1) = mg332_nkj
case (mg5111_id)
 nkj(0:size(mg5111_nkj)-1) = mg5111_nkj
case (mg4211_id)
 nkj(0:size(mg4211_nkj)-1) = mg4211_nkj
case (mg3311_id)
 nkj(0:size(mg3311_nkj)-1) = mg3311_nkj
case (mg3221_id)
 nkj(0:size(mg3221_nkj)-1) = mg3221_nkj
case (mg2222_id)
 nkj(0:size(mg2222_nkj)-1) = mg2222_nkj
case (mg41111_id)
 nkj(0:size(mg41111_nkj)-1) = mg41111_nkj
case (mg32111_id)
 nkj(0:size(mg32111_nkj)-1) = mg32111_nkj
case (mg22211_id)
 nkj(0:size(mg22211_nkj)-1) = mg22211_nkj
case (mg311111_id)
 nkj(0:size(mg311111_nkj)-1) = mg311111_nkj
case (mg221111_id)
 nkj(0:size(mg221111_nkj)-1) = mg221111_nkj
case (mg2111111_id)
 nkj(0:size(mg2111111_nkj)-1) = mg2111111_nkj
case (mg11111111_id)
 nkj(0:size(mg11111111_nkj)-1) = mg11111111_nkj
!! case (mg9_id)
!!  nkj(0:size(mg9_nkj)-1) = mg9_nkj
!! case (mg81_id)
!!  nkj(0:size(mg81_nkj)-1) = mg81_nkj
case (mg72_id)
 nkj(0:size(mg72_nkj)-1) = mg72_nkj
case (mg63_id)
 nkj(0:size(mg63_nkj)-1) = mg63_nkj
case (mg54_id)
 nkj(0:size(mg54_nkj)-1) = mg54_nkj
case (mg711_id)
 nkj(0:size(mg711_nkj)-1) = mg711_nkj
case (mg621_id)
 nkj(0:size(mg621_nkj)-1) = mg621_nkj
case (mg531_id)
 nkj(0:size(mg531_nkj)-1) = mg531_nkj
case (mg441_id)
 nkj(0:size(mg441_nkj)-1) = mg441_nkj
case (mg522_id)
 nkj(0:size(mg522_nkj)-1) = mg522_nkj
case (mg432_id)
 nkj(0:size(mg432_nkj)-1) = mg432_nkj
case (mg333_id)
 nkj(0:size(mg333_nkj)-1) = mg333_nkj
case (mg6111_id)
 nkj(0:size(mg6111_nkj)-1) = mg6111_nkj
case (mg5211_id)
 nkj(0:size(mg5211_nkj)-1) = mg5211_nkj
case (mg4311_id)
 nkj(0:size(mg4311_nkj)-1) = mg4311_nkj
case (mg4221_id)
 nkj(0:size(mg4221_nkj)-1) = mg4221_nkj
case (mg3321_id)
 nkj(0:size(mg3321_nkj)-1) = mg3321_nkj
case (mg3222_id)
 nkj(0:size(mg3222_nkj)-1) = mg3222_nkj
case (mg51111_id)
 nkj(0:size(mg51111_nkj)-1) = mg51111_nkj
case (mg42111_id)
 nkj(0:size(mg42111_nkj)-1) = mg42111_nkj
case (mg33111_id)
 nkj(0:size(mg33111_nkj)-1) = mg33111_nkj
case (mg32211_id)
 nkj(0:size(mg32211_nkj)-1) = mg32211_nkj
case (mg22221_id)
 nkj(0:size(mg22221_nkj)-1) = mg22221_nkj
case (mg411111_id)
 nkj(0:size(mg411111_nkj)-1) = mg411111_nkj
case (mg321111_id)
 nkj(0:size(mg321111_nkj)-1) = mg321111_nkj
case (mg222111_id)
 nkj(0:size(mg222111_nkj)-1) = mg222111_nkj
case (mg3111111_id)
 nkj(0:size(mg3111111_nkj)-1) = mg3111111_nkj
case (mg2211111_id)
 nkj(0:size(mg2211111_nkj)-1) = mg2211111_nkj
case (mg21111111_id)
 nkj(0:size(mg21111111_nkj)-1) = mg21111111_nkj
case (mg111111111_id)
 nkj(0:size(mg111111111_nkj)-1) = mg111111111_nkj
case default
 nkj = -huge(0)
end select
return
END SUBROUTINE mgy_nkj
