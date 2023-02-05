# -*- coding: utf-8 -*-\n
import requests,re
import xml.etree.ElementTree as elemTree

class Calculator:
    def __init__(self):
      # sunrise, sunset time list
        self.sunrise = []
        self.sunset = []
        self.spring_m = 0
        self.spring_d = 0
        self.summer_m = 0
        self.summer_d = 0
        self.fall_m = 0
        self.fall_d = 0
        self.winter_m = 0
        self.winter_d = 0
        self.spring_dayTime = 0
        self.summer_dayTime = 0
        self.fall_dayTime = 0
        self.winter_dayTime = 0

    def set_season(self, type):
      # case 1
      if type == 1:
        self.spring_m = 3
        self.spring_d = 1
        self.summer_m = 6
        self.summer_d = 1
        self.fall_m = 9
        self.fall_d = 1
        self.winter_m = 12
        self.winter_d = 1
      # case 2
      elif type == 2:
        self.spring_m = 2
        self.spring_d = 4
        self.summer_m = 5
        self.summer_d = 5
        self.fall_m = 8
        self.fall_d = 7
        self.winter_m = 11
        self.winter_d = 7
      # case 3  
      elif type == 3:
        self.spring_m = 1
        self.spring_d = 24
        self.summer_m = 4
        self.summer_d = 27
        self.fall_m = 8
        self.fall_d = 19
        self.winter_m = 11
        self.winter_d = 1
 
    def check_season(self, mm, dd):
      #spring 
      if self.spring_m <= mm and mm <= self.summer_m:
        if self.spring_m == mm:
          if self.spring_d <= dd :
            return 'spring'
          else :
            return 'winter'
        elif self.spring_m < mm and mm < self.summer_m :
          return 'spring'
        elif self.summer_m == mm:
          if self.summer_d > dd:
            return 'spring'
          else :
            return 'summer'
      #summer
      if self.summer_m <= mm and mm <= self.fall_m:
        if self.summer_m == mm:
          if self.summer_d <= dd :
            return 'summer'
          else :
            return 'spring'
        elif self.summer_m < mm and mm < self.fall_m :
          return 'summer'
        elif self.fall_m == mm:
          if self.fall_d > dd:
            return 'summer'
          else :
            return 'fall'
      #fall
      if self.fall_m <= mm and mm <= self.winter_m:
        if self.fall_m == mm:
          if self.fall_d <= dd :
            return 'fall'
          else :
            return 'summer'
        elif self.fall_m < mm and mm < self.winter_m :
          return 'fall'
        elif self.winter_m == mm:
          if self.winter_d > dd:
            return 'fall'
          else :
            return 'winter'
            
      #winter
      if self.winter_m <= mm or mm <= self.spring_m:
        if self.winter_m == mm:
          if self.winter_d <= dd :
            return 'winter'
          else :
            return 'fall'
        elif self.winter_m < mm and mm <= 12 :
          return 'winter'
        elif self.spring_m > mm:
          return 'winter'
        elif self.spring_m == mm:
          if self.spring_d > dd:
            return 'winter'
          else :
            return 'spring'
        
cal = Calculator()
cal.set_season(3)
result = cal.check_season(1,28)
print(result)
        
spring_m = 0
spring_c = 0
summer_m = 0
summer_c = 0
fall_m = 0
fall_c = 0
winter_m = 0
winter_c = 0
        
# api address
url = 'http://apis.data.go.kr/B090041/openapi/service/RiseSetInfoService/getAreaRiseSetInfo'
      
#count 365 days (1.1 ~ 12.31)
for mm in range(1,13):
    end_day = 30
    if (mm in [1,3,5,7,8,10,12] ):
        end_day = 31
    elif (mm == 2):
        end_day = 28
    else :
        end_day = 30
    for dd in range(1,end_day+1):
        if mm < 10:
            month = "0"+str(mm)
        else :
            month = str(mm)
            
        if dd < 10:
            day = "0"+str(mm)
        else :
            day = str(dd)
            
        locdate = '2017' + month + day

        params ={'serviceKey' : 'API 인증키', 
                 'locdate' : locdate, 
                 'location' : '안산' }
        
        response = requests.get(url, params=params)
        xmlStr = response.text
        tree = elemTree.fromstring(xmlStr)
        
        sunr = str(tree[1][0][0][15].text)
        suns = str(tree[1][0][0][16].text)
                

        sunr_h = int(sunr[0:2])
        sunr_m = int(sunr[2:4])
        suns_h = int(suns[0:2])
        suns_m = int(suns[2:4])
        
        result = cal.check_season(mm,dd)

        if result == 'spring':
          spring_c += 1
          if suns_m < sunr_m:
            suns_h = suns_h-1
            suns_m = suns_m+60
          spring_m = spring_m + (suns_h - sunr_h)*60 + (suns_m - sunr_m)
        elif result == 'summer':
          summer_c += 1
          if suns_m < sunr_m:
            suns_h = suns_h-1
            suns_m = suns_m+60
          summer_m = summer_m + (suns_h - sunr_h)*60 + (suns_m - sunr_m)
        elif result == 'fall':
          fall_c += 1
          if suns_m < sunr_m:
            suns_h = suns_h-1
            suns_m = suns_m+60
          fall_m = fall_m +(suns_h - sunr_h)*60 + (suns_m - sunr_m)
        elif result == 'winter':
          winter_c += 1
          if suns_m < sunr_m:
            suns_h = suns_h-1
            suns_m = suns_m+60
          winter_m = winter_m+ (suns_h - sunr_h)*60 + (suns_m - sunr_m)\
        
print(spring_m,spring_c)
print(summer_m,summer_c)
print(fall_m,fall_c)
print(winter_m,winter_c)
        
print(spring_m / spring_c / 60)
print(summer_m / summer_c / 60)
print(fall_m / fall_c / 60)
print(winter_m / winter_c / 60)