ifeq ($(shell which protoc),)
	$(error Did not find protoc in PATH. Make sure all requirements are installed)
endif

base_dir=$(abspath .)
java_dir=$(base_dir)/java
cpp_dir=$(base_dir)/cpp
proto_dir=$(base_dir)/proto

.PHONY: default
default:
	@echo "Please specify a file to build"

.PHONY: phony_explicit
phony_explicit:

%.proto: phony_explicit
	@echo "Building Java"
	protoc --java_out=$(java_dir) --proto_path=$(proto_dir) $(base_dir)/$@
	@echo "Building C++"
	protoc --cpp_out=$(cpp_dir) --proto_path=$(proto_dir) $(base_dir)/$@

clean:
	rm -rf $(java_dir)/*
	rm -rf $(cpp_dir)/*

