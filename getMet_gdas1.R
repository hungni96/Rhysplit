library(openair)
# Link 2: https://www.arl.noaa.gov/data/archives/gdas1/gdas1.aug21.w5
# Func.
getMet.gdas1 <- function (start = "2021-07-20 00:00:00",
                          end = "2021-07-29 23:00:00",
                          pathMet = "D:/Rprojects/Hysplit/data",
                          offset = 14) {
  # Xac dinh chuoi thoi gian tai du lieu
  # time.series <- seq(as.POSIXct(start, "GMT"), as.POSIXct(end, "GMT"), by = "1 hour")
  # Xac dinh chuoi ngay de chon tap tin du lieu - tai du lieu theo chuoi thoi gian sau
  # Thoi gian bu mac dinh = 14 (do dai trung binh cua 1 tap tin la 14 ngay)
  date.series <- data.frame(date = seq(as.Date(time.series[1]-3600*24*offset, "GMT"),
                                       as.Date(time.series[length(time.series)]+3600*24*offset, "GMT"), by = "1 day"))
  # Xac dinh cac thanh phan cua ten tap tin
  # Nam (2 chu so)
  date.series$yr <- as.numeric(format(date.series$date, "%y"))
  # Thang (bang chu, viet thuong)
  date.series$mon <- tolower(format(date.series$date, "%b"))
  # Tuan (.w), sap xep cac ngay vao tuan tuong ung
  date.series$week <- as.numeric(format(date.series$date, "%d"))
  date.series$week[date.series$week <= 7] <- 1
  date.series$week[date.series$week >= 8 & date.series$week <= 14] <- 2
  date.series$week[date.series$week >= 15 & date.series$week <= 21] <- 3
  date.series$week[date.series$week >= 22 & date.series$week <= 28] <- 4
  date.series$week[date.series$week >= 29] <- 5
  # Xac dinh chuoi tap tin du lieu
  # Nhung ngay co cung so thu tu tuan (week) se duoc loai bo thong qua bien logic (lg)
  date.series$lg <- NA
  week.number <- data.frame(table(date.series$week))
  for (j in 1:length(week.number$Var1)) {
    date.series$lg[which(date.series$week == week.number$Var1[j])[1]] <- 1
  }
  date.series <- dplyr::filter(date.series, !is.na(lg) & lg > 0)
  date.series <- date.series[,-5]
  # Run download
  for (i in 1:length(date.series$date)) {
    # Kiem tra xem trong thu muc da co tap tin ton tai khong, neu co thi bo qua
    check.met <- file.exists(file.path(pathMet,
                                       paste0("gdas1.", date.series$mon[i], sprintf("%02d", date.series$yr[i]), ".w", date.series$week[i])))
    if (!check.met) {
      skip <- FALSE
      tryCatch({
        download.file(url = paste0("ftp://arlftp.arlhq.noaa.gov/archives/gdas1/gdas1.", date.series$mon[i], sprintf("%02d", date.series$yr[i]), ".w", date.series$week[i]),
                      destfile = file.path(pathMet, 
                                           paste0("gdas1.", date.series$mon[i], sprintf("%02d", date.series$yr[i]), ".w", date.series$week[i])),
                      mode = "wb")
      },
      error = function(e){
        cat("^^! Error:",conditionMessage(e), "\n")
        skip <<- TRUE
      })
      if(skip == TRUE) { next }
    }
  }
}
# save.image(file = file.path(getwd(),"getMet_gdas1.Rdata"))
# RUN
getMet.gdas1(start = "2021-07-20 00:00:00",
             end = "2021-07-29 23:00:00",
             pathMet = "D:/Rprojects/Hysplit/data",
             offset = 7)


