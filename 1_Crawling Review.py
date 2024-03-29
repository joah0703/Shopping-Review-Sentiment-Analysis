# python code
from selenium import webdriver #셀레니움 라이브러리 사용
from openpyxl import Workbook
import time as t
import random as r
comment_real=[]
comment=[]

driver = webdriver.Chrome('C:/chromedriver.exe') #구글 드라이버 실행

driver.get('https://search.shopping.naver.com/best100v2/detail.nhn?catId=50000008&listType=B10002') #카테고리별 url

write_wb = Workbook()
write_ws = write_wb.active

write_wa = Workbook()
write_wc = write_wa.active
for i  in range(68,100):
    try:
        t.sleep(r.randint(2, 3))
        driver.find_element_by_xpath('//*[@id="productListArea"]/ul/li['+str(i+1)+']').click() #상품 클릭

        t.sleep(r.uniform(1, 2))
        driver.switch_to.window(driver.window_handles[-1])

        positive = []
        negative = []

        try:
            driver.find_element_by_xpath('//*[@id="_score_filter"]/li[2]/a').click() #별점 5점 클릭
            t.sleep(r.uniform(1,2))
            for j in range(len(driver.find_elements_by_class_name('atc'))):
                positive.append(driver.find_elements_by_class_name('atc')[j].text) #후기 크롤링
        except:
            print("5")

        try:
            driver.find_element_by_xpath('//*[@id="_score_filter"]/li[3]/a').click() #별점 4점 클릭
            t.sleep(r.uniform(1,2))
            for j in range(len(driver.find_elements_by_class_name('atc'))):
                positive.append(driver.find_elements_by_class_name('atc')[j].text)
        except:
            print("4")

        try:
            driver.find_element_by_xpath('//*[@id="_score_filter"]/li[5]/a').click() #별점 2점 클릭
            t.sleep(r.uniform(1,2))
            for j in range(len(driver.find_elements_by_class_name('atc'))):
                negative.append(driver.find_elements_by_class_name('atc')[j].text)
        except:
            print("2")

        try:
            driver.find_element_by_xpath('//*[@id="_score_filter"]/li[6]/a').click() #별점 1점 클릭
            t.sleep(r.uniform(1,2))
            for j in range(len(driver.find_elements_by_class_name('atc'))):
                negative.append(driver.find_elements_by_class_name('atc')[j].text)
        except:
            print("1")


        for j in range(len(positive)):
            write_ws.append([positive[j]])

        for j in range(len(negative)):
            write_wc.append([negative[j]])

        driver.close()

        t.sleep(r.uniform(2, 3))

        driver.switch_to.window(driver.window_handles[0])

        write_wb.save('C:/navershopping/positive_생활건강.xlsx')
        write_wa.save('C:/navershopping/negative_생활건강.xlsx')
    except:
        print(i)

driver.quit()
