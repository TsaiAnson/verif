#!/bin/bash

if [[ "$OSTYPE" == "darwin"* ]]
then
  script=$(greadlink -f -- "$0")
else
  script=$(readlink -f -- "$0")
fi

base_dir=$(dirname "$script")

chipyard_dir="$base_dir/../../.."
spike_dir="$chipyard_dir/toolchains/esp-tools/riscv-isa-sim/"
gemmini_extension_dir="$spike_dir/gemmini"

resources_dir="$base_dir/resources"
proto_dir="$resources_dir/proto"
java_dir="$resources_dir/java"
cpp_dir="$resources_dir/cpp"

echo "Building protocol buffer files"
for proto in $(ls "$proto_dir")
do
  protoc --java_out="$java_dir" --proto_path="$proto_dir" "$proto_dir"/"$proto"
  protoc --cpp_out="$cpp_dir" --proto_path="$proto_dir" "$proto_dir"/"$proto"
done

echo "Copying generated files to their proper locations"
for f in $(ls "$cpp_dir")
do
  cp "$cpp_dir"/"$f" "$gemmini_extension_dir"/
done


echo "Building Spike"
cd "$spike_dir"
rm -rf build
mkdir build
cd build
../configure --prefix=$RISCV
if [[ -n "$COSIM_PROTOBUF_LOCAL" ]]
then
  sed -i "s+-lprotobuf+-L"$base_dir"/install/lib -lprotobuf+" Makefile
fi
make && make install

