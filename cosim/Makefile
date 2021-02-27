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
	git checkout 2802ca406323ef511e1c2939387144a47c250638; \
	cd $(gemmini_sim_dir); \
	git remote add cosim https://github.com/ryan-lund/esp-isa-sim.git; \
	git fetch cosim
	git checkout custom --track cosim/master; \
	git pull cosim master; \
	cd build; \
	make && make install
	@echo "Sim setup done"

gemmini-sim-build:
	@echo "Building Gemmini RoCC tests"
	cd $(gemmini_rocc_tests_dir); \
	./build.sh
	@echo "Build completed"
	@echo "If you have not elaborated Gemmini with your config, set your configuration in CosimTest.scala"
	@echo "Then execute the following and re-run this target"
	@echo "sbt> project verif.Gemmini"
	@echo 'sbt> testOnly verif.CosimTest -- -z "elaborate for parameters"'

