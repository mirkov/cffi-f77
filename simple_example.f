      SUBROUTINE EX1 (XI, YI, ZO)
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      open (unit=7, file='ex1.echo',status='replace')
      zo = xi+yi
      write (7,*) xi, yi
      write (7,*) zo
      close (7)
      RETURN
      END
