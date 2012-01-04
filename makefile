fortran = g77

objs = simple_example.o subroutines.o

%.o : %.f
	$(fortran) -c -fPIC $< -o $@

special : clean
clean :
	rm -f $(objs)

library.dll : simple_example.o $(objs)
	$(fortran) -shared  -o library.dll $(objs)

special : clean_lib
clean_lib :
	rm -f library.dll
