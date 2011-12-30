      subroutine logical_test (flag)
      logical flag
      open (unit=7, file='logical_test.echo',status='replace')
      write (7,*) flag
      flag = .not. flag
      write (7,*) flag
      close (7)
      return
      end

      subroutine integer_test (input, output)
      integer*4 input, output
      open (unit=7, file='integer_test.echo',status='replace')
      output=input
      write (7,*) input, output
      close (7)
      return
      end

      subroutine real_test (input, output)
      real*4 input, output
      open (unit=7, file='real_test.echo',status='replace')
      output=input
      write (7,*) input, output
      close (7)
      return
      end

      subroutine double_test (input, output)
      real*8 input, output
      open (unit=7, file='double_test.echo',status='replace')
      output=input
      write (7,*) input, output
      close (7)
      return
      end

      subroutine complex_test (input, output)
      complex input, output
      open (unit=7, file='complex_test.echo',status='replace')
      output=conjg(input)
      write (7,*) input, output
      close (7)
      return
      end

      subroutine char_test (input, output)
      character*(*) input, output
      open (unit=7, file='char_test.echo',status='replace')
      output=input
      write (7,*) input, output
      close (7)
      return
      end

      subroutine real_arr_test (input, output,n)
      real*4 input, output
      integer n
      dimension input(n), output(n)
      open (unit=7, file='real_test.echo',status='replace')
      do i=1,n
         output(i)=2.0*input(i)
      enddo
      write (7,*) input, output
      close (7)
      return
      end

      subroutine complex_arr_test (input, output,n)
      complex input, output
      integer n
      dimension input(n), output(n)
      open (unit=7, file='real_test.echo',status='replace')
      do i=1,n
         output(i)=complex (2.0*real(input(i)), -imag(input(i))) 
      enddo
      write (7,*) input, output
      close (7)
      return
      end
