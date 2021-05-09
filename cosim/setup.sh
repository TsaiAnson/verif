#!/bin/bash

base_dir="$(pwd)"
install_dir="$base_dir/install"

chipyard_dir="$base_dir/../../.."
spike_dir="$chipyard_dir/toolchains/esp-tools/riscv-isa-sim/"
gemmini_extension_dir="$spike_dir/gemmini"

resources_dir="$base_dir/resources"
proto_dir="$resources_dir/proto"
java_dir="$resources_dir/java"
cpp_dir="$resources_dir/cpp"

proto_version="0.0"
if command -v protoc &> /dev/null
then
  proto_version=$(protoc --version | awk '{print $2}')
fi

proto_major=$(echo $proto_version | cut -f1 -d.)
proto_minor=$(echo $proto_version | cut -f2 -d.)

if [[ $proto_major -ge 3 && $proto_minor -ge 3 ]];
then
  echo "Installed protoc meets requirements"
  PROTOC=protoc
else
  echo "Installed protoc does not meet requirements"
  echo "Installing protoc 3.3.0 locally"
  mkdir -p $install_dir
  cd $install_dir
  wget https://github.com/protocolbuffers/protobuf/releases/download/v3.3.0/protobuf-java-3.3.0.zip
  unzip protobuf-java-3.3.0.zip
  rm protobuf-java-3.3.0.zip
  cd protobuf-3.3.0
  ./configure --prefix=$install_dir
  make
  make check
  make install
  PROTOC=$install_dir/bin/protoc
fi

#echo "Building protocol buffer files"
#for proto in $(ls $proto_dir)
#do
#  $PROTOC --java_out=$java_dir --proto_path=$proto_dir $proto_dir/$proto
#  $PROTOC --cpp_out=$cpp_dir --proto_path=$proto_dir $proto_dir/$proto
#done

#echo "Copying generated files to their proper locations"
#for f in $(ls $cpp_dir)
#do
#  cp $cpp_dir/$f $gemmini_extension_dir/
#done

#echo "Building Spike"
#cd $spike_dir
#mkdir build
#cd build
#../configure --prefix=$RISCV
#make && make install
