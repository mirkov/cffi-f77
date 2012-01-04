fortran = g77

objs = simple_example.o subroutines.o

%.o : %.f
	$(fortran) -c -fPIC $< -o $@

special : clean
clean :
	rm -f $(objs)

libf77ex.dll : $(objs)
	$(fortran) -shared  -o libf77ex.dll $(objs)

libf77ex.so : $(objs)
	$(fortran) -shared -o libf77ex.so $(objs)

special : clean_lib
clean_lib :
	rm -f library.dll
