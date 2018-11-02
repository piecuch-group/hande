      subroutine NJ_walltim(io,starting_time)
      implicit none
      integer io
      integer(kind=4) elapsed_time, starting_time
      integer(kind=8) ndays, nhours, nmin
      real(kind=4) values(2)
      real(kind=4) time

 1000  Format(2x,'Wall time:',I3,' days ',I2,' hours ',I2,' minutes ',
     $    I4,' seconds.')

      call etime(values, time)
      elapsed_time = int(time,kind=8)-starting_time
      NDays= elapsed_time/(3600*24)
      elapsed_time = elapsed_time-NDays*(3600*24)
      NHours= elapsed_time/3600
      elapsed_time = elapsed_time-NHours*3600
      NMin = elapsed_time/60
      elapsed_time = elapsed_time-NMin*60
      Write(io,1000) NDays, NHours, NMin, elapsed_time

      Return

      End


