
cosim_dir=$(abspath .)
resources_dir=$(cosim_dir)/resources
cpp_dir=$(resources_dir)/cpp
chipyard_root_dir=$(cosim_dir)/../../..
gemmini_sim_dir=$(chipyard_root_dir)/toolchains/esp-tools/riscv-isa-sim
gemmini_dir=$(chipyard_root_dir)/generators/gemmini
gemmini_rocc_tests_dir=$(gemmini_dir)/software/gemmini-rocc-tests/

.PHONY: gemmini-sim-setup gemmini-sim-build
gemmini-sim-setup:
	cd $(gemmini_dir); \
	git checkout blas; \
	git pull origin blas
	cd $(gemmini_rocc_tests_dir); \
	git checkout dev; \
	git pull origin dev
	cd $(gemmini_sim_dir); \
	git remote add cosim https://github.com/ryan-lund/esp-isa-sim.git; \
	git fetch cosim
	git checkout cosim/master; \
	git pull cosim master

gemmini-sim-build:
	@echo "Creating fresh run directory"
	rm -rf $(cosim_dir)/cosim_run_dir
	mkdir $(cosim_dir)/cosim_run_dir
	@echo "Modifying gemmini_params.h for cosim"
	echo "#define COSIM" >> $(gemmini_rocc_tests_dir)/include/gemmini_params.h
	echo "#define COSIM_PATH $(cosim_dir)/cosim_run_dir" >> $(gemmini_rocc_tests_dir)/include/gemmini_params.h
	@echo "Copying necessary files to esp-isa-sim"
	cp $(gemmini_rocc_tests_dir)/include/gemmini_params.h $(gemmini_sim_dir)/gemmini/
	cp $(cpp_dir)/* $(gemmini_sim_dir)/gemmini/
	@echo "Building Gemmini RoCC tests"
	cd $(gemmini_rocc_tests_dir); \
	./build.sh
	@echo "Building esp-isa-sim"
	cd $(gemmini_sim_dir)/build; \
	make && make install
	@echo "Sim setup done"

