#FC		?= ifort
FC := gfortran
FCFLAGS := -O0 -fcheck=all -fdefault-integer-8 -m64 -I${MKLROOT}/include -fbacktrace -g -mtune=native
LDFLAGS := -Wl,--start-group ${MKLROOT}/lib/intel64/libmkl_gf_ilp64.a ${MKLROOT}/lib/intel64/libmkl_gnu_thread.a ${MKLROOT}/lib/intel64/libmkl_core.a -Wl,--end-group -lgomp -lpthread -lm -ldl -luuid

diis_SRCS := $(wildcard libs/DIIS/*.f)
sub_sj_SRCS := $(wildcard libs/sub_sj/*.f)
sub_lw_SRCS := $(wildcard libs/sub_lw/*.f)
cc_SRCS := $(wildcard *.f *.f90)

diis_OBJS := $(diis_SRCS:.f=.o)
sub_sj_OBJS := $(sub_sj_SRCS:.f=.o)
sub_lw_OBJS := $(sub_lw_SRCS:.f=.o)
cc_OBJS := $(cc_SRCS:.f=.o)
cc_OBJS := $(cc_OBJS:.f90=.o)

LIBS = diis.a sub_sj.a sub_lw.a
LIB := cc_p.a

.PHONY: all clean

all: $(LIB)

test: main/main.f $(diis_OBJS) $(sub_sj_OBJS) $(sub_lw_OBJS) $(cc_OBJS)
	$(FC) -o $@ $(FCFLAGS) $^ $(LDFLAGS)

$(LIB): $(diis_OBJS) $(sub_sj_OBJS) $(sub_lw_OBJS) $(cc_OBJS)
	ar -r $@ $^

%.o: %.f90
	$(FC) -c $(FCFLAGS) -o $@ $<

%.o: %.f
	$(FC) -c $(FCFLAGS) -o $@ $<

clean:
	@echo Cleaning...
	rm -f *.a
	rm -f $(diis_OBJS) $(sub_sj_OBJS) $(sub_lw_OBJS) $(cc_OBJS)
	rm -f main/main.o
	@echo Done.
