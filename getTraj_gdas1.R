
procTraj.gdas1 <- function(lat, lon, height,
                           start, end,
                           offset = 14, file.name, hours,
                           pathMet = "D:/Rprojects/Hysplit/data",
                           pathTraj = "D:/Rprojects/Hysplit/traj",
                           pathHys = "C:/hysplit") {
  #' Giai thich
  #' : Co the chon nhieu receptor voi lat, lon, height
  #' : Dinh dang thoi gian mac dinh start, end
  #' : offset la thoi gian bu dam bao khong de bi thieu du lieu
  #' : hours khong vuot qua 7 ngay, co dinh dang + hoac - tuy theo fwrd hoac bwrd
  #' ----------------------------------------------------------------------------------
  lapply(c("openair", "plyr", "reshape2", "readxl"), require, character.only = TRUE)
  setwd(file.path(pathHys, "working"))
  path.files <- file.path(pathHys, "working")
  bat.file   <- file.path(pathHys, "working", "test.bat")
  # Xoa cac tap tin tdump tu truoc
  lapply(list.files(path = path.files, pattern = "tdump"), function(x) file.remove(x))
  # Xac dinh chuoi thoi gian tai du lieu
  time.series <- seq(as.POSIXct(start, "GMT"), as.POSIXct(end, "GMT"), by = "1 hour")
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
  # List receptor
  list.receptor <- data.frame(lat, lon, height)
  # Run BAT
  for (i in 1:length(time.series)) {
    # Dinh dang thoi gian
    # Nam
    year <- format(time.series[i], "%y")  # e.g. chr "19"
    yr   <- as.numeric(year)              # e.g. num 19
    Year <- format(time.series[i], "%Y")  # e.g. chr "2019"
    # Thang
    month <- format(time.series[i], "%m") # e.g. chr "01"
    mon   <- tolower(format(time.series[i], "%b"))
    # Ngay
    day <- format(time.series[i], "%d")   # e.g. num "01"
    # Gio
    hour <- format(time.series[i], "%H")  # e.g. num "01"
    # Tuan
    week <- as.numeric(day)
    if (week %in% 1:7)   {week <- 1}
    if (week %in% 8:14)  {week <- 2}
    if (week %in% 15:21) {week <- 3}
    if (week %in% 22:28) {week <- 4}
    if (week %in% 29:31) {week <- 5}
    
    # Setup---------------------------
    # Setup date and time
    x <- paste("echo", year, month, day, hour, ">CONTROL")
    write.table(x, bat.file, col.names = FALSE, row.names = FALSE, quote = FALSE, append = FALSE)
    
    # Setup receptor
    x <- paste("echo", length(list.receptor$lat), ">>CONTROL")
    write.table(x, bat.file, col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
    
    for (j in 1:length(list.receptor$lat)) {
      x <- paste("echo", list.receptor$lat[j], list.receptor$lon[j], list.receptor$height[j], ">>CONTROL")
      write.table(x, bat.file, col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
    }
    
    # Setup method
    x <- paste("echo", hours, ">>CONTROL")
    write.table(x, bat.file, col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
    
    # vertical motion method 0 = input model data: phuong phap chuyen dong thang dung cua mo hinh, mac dinh se su dung van toc doc tu du lieu khi tuong hoc
    # top of model 15000.0 m agl
    # number of meteorology files
    x <- "echo 0       >>CONTROL
          echo 15000.0 >>CONTROL
          echo 4       >>CONTROL"
    write.table(x, bat.file, col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
    
    # Add meteorology files
    # Row must be greater than 1, which means the minimum period is 3rd week of January 2006
    row.w <- which(date.series$yr == yr & date.series$week == week & date.series$mon == mon)
    ifelse(hours > 0, n <- c(1,2), n <- c(2,1))
    for (k in (row.w - n[1]):(row.w + n[2])) {
      write.table(paste0("echo ", pathMet, "/", " >>CONTROL"), bat.file, col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
      x <- paste0("echo gdas1.", date.series$mon[k], date.series$yr[k], ".w", date.series$week[k], " >>CONTROL") # e.g.gdas1.jan19.w1 
      
      write.table(x, bat.file, col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
    }
    
    # Setup folder: tdump
    x <- paste0("echo ", path.files, "/", " >>CONTROL")
    write.table(x, bat.file, col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
    
    # Setup name: tdump
    x <- paste0("echo tdump", year, month, day, hour, " >>CONTROL") # e.g. tdump19011600
    write.table(x, bat.file, col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
    
    # Set hyst_std path
    x <- file.path(pathHys, "exec", "hyts_std")
    write.table(x, bat.file, col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
    
    # Run test file (test.bat)
    system(file.path(pathHys, "working", "test.bat"))
  }
  # Tao Rcombined
  list.tdump <- Sys.glob("tdump*")
  output <- file('Rcombined.txt', 'w')
  for (mol in list.tdump){
    input <- readLines(mol)
    input <- input[-c(1:(length(list.receptor$lat) + 4 + 3))] # xoa cac dong khai bao
    writeLines(input, output)
  }
  close(output)
  
  # Tao Rdata
  traj <- read.table("Rcombined.txt", header = FALSE)
  traj <- subset(traj, select = -c(V2, V7, V8))
  traj <- rename(traj, c(V1 = "receptor", V3 = "year", V4 = "month", V5 = "day", V6 = "hour",
                         V9 = "hour.inc", V10 = "lat", V11 = "lon", V12 = "height", V13 = "pressure"))
  year <- traj$year[1]
  if (year < 69) { # specified by the 2004 and 2008 POSIX standards
    traj$year <- traj$year + 2000 
  }
  else {
    traj$year <- traj$year + 1900
  }
  traj$date2 <- with(traj, ISOdatetime(year, month, day, hour, min = 0, sec = 0, tz = "GMT"))
  traj$date  <- traj$date2 - 3600 * traj$hour.inc
  save(traj, file = paste0(pathTraj, "/", file.name, ".RData"))
  return(traj)
}
# RUN
traj <- procTraj.gdas1(lat = 21.02, lon = 105.8, height = 500,
                       start = "2021-07-28 00:00:00", end = "2021-07-28 20:00:00",
                       offset = 14, file.name = "hanoi", hours = -24,
                       pathMet = "D:/Rprojects/Hysplit/data",
                       pathTraj = "D:/Rprojects/Hysplit/traj",
                       pathHys = "C:/hysplit")

