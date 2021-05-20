cosim_dir=$(abspath .)
install_dir=$(cosim_dir)/install
resources_dir=$(cosim_dir)/resources

chipyard_root_dir=$(cosim_dir)/../../..
spike_dir=$(chipyard_root_dir)/toolchains/esp-tools/riscv-isa-sim
gemmini_dir=$(chipyard_root_dir)/generators/gemmini
gemmini_rocc_tests_dir=$(gemmini_dir)/software/gemmini-rocc-tests/

.PHONY: setup gemmini-sim-build

setup:
	@echo "Setting up Cosim Resources"
	@echo "Installing Protoc 3.3.0 Locally"
	mkdir -p $(install_dir)
	cd $(install_dir) && \
	git clone https://github.com/protocolbuffers/protobuf.git && \
	cd protobuf && \
	git checkout v3.3.0 && \
	git submodule update --init --recursive && \
	./autogen.sh && \
	./configure --prefix=$(install_dir) && \
	make && \
	make check && \
	make install
	@echo "Building Protocol Buffer Files"
	@echo "Building Java"
	protoc --java_out=$(java_dir) --proto_path=$(proto_dir) *
	@echo "Building C++"
	protoc --cpp_out=$(cpp_dir) --proto_path=$(proto_dir) *


gemmini-sim-build:
	@echo "Building Gemmini RoCC tests"
	cd $(gemmini_rocc_tests_dir); \
	./build.sh
	@echo "Build completed"
	@echo "If you have not elaborated Gemmini with your config, set your configuration in CosimTest.scala"
	@echo "Then execute the following and re-run this target"
	@echo "sbt> project verif.Gemmini"
	@echo 'sbt> testOnly verif.CosimTest -- -z "elaborate for parameters"'

