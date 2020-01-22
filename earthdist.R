earthdist = function(lat1,lon1,lat2,lon2) {
# Translation of /usr/local/idl/user_contrib/esrg_ucsb/earthdist.pro, etc.

  cosd = function(angle) {
    cos(angle*pi/180)
  }
  acosd = function(value) {
    acos(value)*180/pi
  }
  cosscatang = function(cossza, cosvza, phi) {
    # radian_phi = phi * !dtor
    # return, cossza*cosvza +    $
    #         sin(acos(cossza))*sin(acos(cosvza))*cos(radian_phi)
    radian_phi = phi * pi / 180
    cossza*cosvza + sin(acos(cossza))*sin(acos(cosvza))*cos(radian_phi)
  }

  colat1 = 90. - lat1
  colat2 = 90. - lat2

  cossza = cosd(colat1)
  cosvza = cosd(colat2)

  phi = abs(lon1 - lon2)

  great_circle_angle = acosd(cosscatang(cossza,cosvza,phi))

  # return, 6371 * great_circle_angle*!DTOR
  6371 * great_circle_angle*pi/180

}

