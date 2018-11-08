#FC		?= ifort
FC := gfortran
FCFLAGS := -O0 -fcheck=all -fdefault-integer-8 -m64 -I${MKLROOT}/include -fbacktrace -g -mtune=native
LDFLAGS := -Wl,--start-group ${MKLROOT}/lib/intel64/libmkl_gf_ilp64.a ${MKLROOT}/lib/intel64/libmkl_gnu_thread.a ${MKLROOT}/lib/intel64/libmkl_core.a -Wl,--end-group -lgomp -lpthread -lm -ldl -luuid

cc_SRCS := $(wildcard cc/*.f)
hbar_SRCS := $(wildcard hbar/*.f)
lcc_SRCS := $(wildcard lcc/*.f)
corr_SRCS := $(wildcard corr/*.f)
main_SRCS := $(wildcard *.f *.f90 *.F90)

cc_OBJS := $(cc_SRCS:.f=.o)
hbar_OBJS := $(hbar_SRCS:.f=.o)
lcc_OBJS := $(lcc_SRCS:.f=.o)
corr_OBJS := $(corr_SRCS:.f=.o)
main_OBJS := $(main_SRCS:.f=.o)
main_OBJS := $(main_OBJS:.f90=.o)
main_OBJS := $(main_OBJS:.F90=.o)

LIB := cc_p.a

.PHONY: all clean

all: $(LIB)

test: test_dir/main.f test_dir/NJ_outfile.f $(cc_OBJS) $(hbar_OBJS) $(lcc_OBJS) $(corr_OBJS) $(main_OBJS)
	$(FC) -o $@ $(FCFLAGS) $^ $(LDFLAGS)

$(LIB): $(cc_OBJS) $(hbar_OBJS) $(lcc_OBJS) $(corr_OBJS) $(main_OBJS)
	ar -r $@ $^

cc_driver.o: solve_cc.o solve_lcc.o ccp3.o const.o

solve_cc.o: diis.o utils.o

solve_lcc.o: diis.o utils.o const.o

ccp3.o: const.o

printing.o: printing.F90
	$(FC) -c $(FCFLAGS) -o $@ $<

%.o: %.f90
	$(FC) -c $(FCFLAGS) -o $@ $<

%.o: %.f
	$(FC) -c $(FCFLAGS) -o $@ $<

%.mod: ;

clean:
	@echo Cleaning...
	rm -f *.a
	rm -f *.mod */*.mod
	rm -f $(cc_OBJS) $(hbar_OBJS) $(lcc_OBJS) $(main_OBJS)
	rm -f main/main.o
	@echo Done.
