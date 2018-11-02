      subroutine NJ_cputim(IOut,RefTim)
      Implicit Real*8(A-H,O-Z)

 1000  format('CPU time: ',I3,' days ',I2,' hours ',I2,' minutes ',
     $    F4.1,' seconds.')

      Time = CPUTim(0) - RefTim
      NDays = (Time / (3600.0d0*24.0d0))
      Time = Time - (NDays*(3600.0d0*24.0d0))
      NHours = (Time / 3600.0d0)
      Time = Time - (NHours*3600.0d0)
      NMin = (Time / 60.0d0)
      Time = Time - (NMin*60.0d0)
      Write(IOut,1000) NDays, NHours, NMin, Time
      Return
      End

      function CPUTim(Junk)
      Implicit Real*8(a-h,o-z)
C
C#ifdef IBM_RS6K
C#define CPUTIM_DONE
C      Integer IT(4), Times
C      IDum = Times(IT)
C      CPUTim = DFloat(IT(1)+IT(2))/100.0d0
C#endif
C#ifdef IBM_PC
C#define CPUTIM_DONE
C      CPUTim = PCTIME ()
C#endif
C#ifndef CPUTIM_DONE
C#ifdef _SGI64_
C      Real*4 TimArray(2), ETime
C#else
      Real TimArray(2), ETime
C#endif
      CPUTim = ETime(TimArray)
C#endif
      Return
      End

