## Подготовка данных 1.. Импортируйте данные DNS

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    inf = read.csv("dns.log", header = FALSE,sep = "\t",encoding = "UTF-8")

## 2. Добавьте пропущенные данные о структуре данных (назначении столбцов)

    head=read.csv("header.csv", header = TRUE)

    field<-head[,1]
    inf = read.csv("dns.log", header = FALSE,sep = "\t",encoding = "UTF-8")
    names(inf)<-field
    inf%>%glimpse()

    ## Rows: 427,935
    ## Columns: 23
    ## $ `ts `          <dbl> 1331901006, 1331901015, 1331901016, 1331901017, 1331901…
    ## $ `uid `         <chr> "CWGtK431H9XuaTN4fi", "C36a282Jljz7BsbGH", "C36a282Jljz…
    ## $ `orig_ip `     <chr> "192.168.202.100", "192.168.202.76", "192.168.202.76", …
    ## $ `orig_port `   <int> 45658, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137…
    ## $ `resp_ip `     <chr> "192.168.27.203", "192.168.202.255", "192.168.202.255",…
    ## $ `resp_port `   <int> 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, …
    ## $ `proto `       <chr> "udp", "udp", "udp", "udp", "udp", "udp", "udp", "udp",…
    ## $ `trans_id `    <int> 33008, 57402, 57402, 57402, 57398, 57398, 57398, 62187,…
    ## $ `query `       <chr> "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x…
    ## $ `qclass `      <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", …
    ## $ `qclass_name ` <chr> "C_INTERNET", "C_INTERNET", "C_INTERNET", "C_INTERNET",…
    ## $ `qtype `       <chr> "33", "32", "32", "32", "32", "32", "32", "32", "32", "…
    ## $ `qtype_name `  <chr> "SRV", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", …
    ## $ `rcode `       <chr> "0", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", …
    ## $ `rcode_name `  <chr> "NOERROR", "-", "-", "-", "-", "-", "-", "-", "-", "-",…
    ## $ `QR `          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
    ## $ `AA `          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
    ## $ `TC RD `       <lgl> FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, …
    ## $ `RA `          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
    ## $ `Z `           <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1…
    ## $ `answers `     <chr> "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", …
    ## $ `TTLs `        <chr> "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", …
    ## $ `rejected `    <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…

## 3. Преобразуйте данные в столбцах в нужный формат

## Анализ 4. Сколько участников информационного обмена в сети Доброй Организации?

    orig_ip<-unique(inf$`orig_ip `)
    resp_ip<-unique(inf$`resp_ip `)
    all_ip<-merge(orig_ip,resp_ip)
    NROW(unique(all_ip$`x`))

    ## [1] 253

## 5. Какое соотношение участников обмена внутри сети и участников обращений к внешним ресурсам?

    m51<-unique(inf$`orig_ip `,sort=TRUE)
    m52<-unique(inf$`resp_ip `,sort=TRUE)
    new_vector <- c(m51, m52)
    new_vector<-new_vector[!duplicated(new_vector)]
    lall=length(new_vector)
    toMatch <- c("(192.168.)([0-9]{1,3}[.])[0-9]{1,3}","(10.0.)([0-9]{1,3}[.])[0-9]{1,3}","(100.64.)([0-9]{1,3}[.])[0-9]{1,3}","(172.16.)([0-9]{1,3}[.])[0-9]{1,3}")
    l1=length(unique(grep(paste(toMatch,collapse="|"), 
                            new_vector, value=TRUE)))
    l2=lall-l1
    contr=l1/l2
    contr

    ## [1] 11.94286

## 6. Найдите топ-10 участников сети, проявляющих наибольшую сетевую активность.

    m1<-inf%>%
    count(inf$`orig_ip `,sort=TRUE)
    colnames(m1) <- c("Person", "count")
    m2<-inf%>%
    count(inf$`resp_ip `,sort=TRUE)
    colnames(m2) <- c("Person", "count")
    mm<-rbind(m1,m2)
    mm<-mm %>%
      group_by(Person) %>%
      summarise(count)

    ## `summarise()` has grouped output by 'Person'. You can override using the
    ## `.groups` argument.

    mm<-mm[order(mm$Person, decreasing = TRUE), ]   
    mm

    ## # A tibble: 1,483 × 2
    ## # Groups:   Person [1,359]
    ##    Person                    count
    ##    <chr>                     <int>
    ##  1 ff02::fb                   3298
    ##  2 ff02::1:3                 14411
    ##  3 fec0:0:0:ffff::3             44
    ##  4 fec0:0:0:ffff::2             47
    ##  5 fec0:0:0:ffff::1             47
    ##  6 fe80::f2de:f1ff:fe9b:ad6a    30
    ##  7 fe80::d840:5635:ef48:b032   276
    ##  8 fe80::d69a:20ff:fef9:b49c   141
    ##  9 fe80::d69a:20ff:fef9:b49c     2
    ## 10 fe80::c62c:3ff:fe37:efc     126
    ## # … with 1,473 more rows

## 7. Найдите топ-10 доменов, к которым обращаются пользователи сети и соответственное количество обращений

    m7<-inf%>%
    count(inf$`query `,sort=TRUE)
    m7head<-m7%>%
    head(10)
    m7head

    ##                                                               inf$`query `
    ## 1                                                teredo.ipv6.microsoft.com
    ## 2                                                         tools.google.com
    ## 3                                                            www.apple.com
    ## 4                                                           time.apple.com
    ## 5                                          safebrowsing.clients.google.com
    ## 6  *\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00
    ## 7                                                                     WPAD
    ## 8                                              44.206.168.192.in-addr.arpa
    ## 9                                                                 HPE8AA67
    ## 10                                                                  ISATAP
    ##        n
    ## 1  39273
    ## 2  14057
    ## 3  13390
    ## 4  13109
    ## 5  11658
    ## 6  10401
    ## 7   9134
    ## 8   7248
    ## 9   6929
    ## 10  6569

## 8. Опеределите базовые статистические характеристики (функция summary()) интервала времени между последовательным обращениями к топ-10 доменам.

    m7head%>%summary()

    ##  inf$`query `             n        
    ##  Length:10          Min.   : 6569  
    ##  Class :character   1st Qu.: 7720  
    ##  Mode  :character   Median :11030  
    ##                     Mean   :13177  
    ##                     3rd Qu.:13320  
    ##                     Max.   :39273

## 9. Часто вредоносное программное обеспечение использует DNS канал в качестве канала управления, периодически отправляя запросы на подконтрольный злоумышленникам DNS сервер. По периодическим запросам на один и тот же домен можно выявить скрытый DNS канал. Есть ли такие IP адреса в исследуемом датасете?

    ms<-inf[,c("orig_ip ","query ")]
    m8<-ms
    m8%>%group_by(m8$`query `)

    ## # A tibble: 427,935 × 3
    ## # Groups:   m8$`query ` [5,178]
    ##    `orig_ip `      `query `                                              m8$\q…¹
    ##    <chr>           <chr>                                                 <chr>  
    ##  1 192.168.202.100 "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00… "*\\x0…
    ##  2 192.168.202.76  "HPE8AA67"                                            "HPE8A…
    ##  3 192.168.202.76  "HPE8AA67"                                            "HPE8A…
    ##  4 192.168.202.76  "HPE8AA67"                                            "HPE8A…
    ##  5 192.168.202.76  "WPAD"                                                "WPAD" 
    ##  6 192.168.202.76  "WPAD"                                                "WPAD" 
    ##  7 192.168.202.76  "WPAD"                                                "WPAD" 
    ##  8 192.168.202.89  "EWREP1"                                              "EWREP…
    ##  9 192.168.202.89  "EWREP1"                                              "EWREP…
    ## 10 192.168.202.89  "EWREP1"                                              "EWREP…
    ## # … with 427,925 more rows, and abbreviated variable name ¹​`m8$\`query \``

    m8<-m8%>%group_by(m8$orig_ip ) %>%count(m8$`query `,sort=TRUE)
    m8<-m8%>%count(m8$`m8$orig_ip`,)
    m8<-m8[which(m8$n==1),]
    m8[,c('m8$orig_ip')]

    ## # A tibble: 78 × 1
    ## # Groups:   m8$orig_ip [78]
    ##    `m8$orig_ip`   
    ##    <chr>          
    ##  1 128.244.172.252
    ##  2 172.16.10.130  
    ##  3 192.168.100.130
    ##  4 192.168.202.116
    ##  5 192.168.207.58 
    ##  6 192.168.208.18 
    ##  7 192.168.21.1   
    ##  8 192.168.21.100 
    ##  9 192.168.21.102 
    ## 10 192.168.21.202 
    ## # … with 68 more rows

## Обогащение данных 10. Определите местоположение (страну, город) и организацию-провайдера для топ-10 доменов. Для этого можно использовать сторонние сервисы, например <https://v4.ifconfig.co/>.
