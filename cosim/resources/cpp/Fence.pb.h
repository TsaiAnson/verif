// Generated by the protocol buffer compiler.  DO NOT EDIT!
// source: Fence.proto

#ifndef GOOGLE_PROTOBUF_INCLUDED_Fence_2eproto
#define GOOGLE_PROTOBUF_INCLUDED_Fence_2eproto

#include <limits>
#include <string>

#include <google/protobuf/port_def.inc>
#if PROTOBUF_VERSION < 3014000
#error This file was generated by a newer version of protoc which is
#error incompatible with your Protocol Buffer headers. Please update
#error your headers.
#endif
#if 3014000 < PROTOBUF_MIN_PROTOC_VERSION
#error This file was generated by an older version of protoc which is
#error incompatible with your Protocol Buffer headers. Please
#error regenerate this file with a newer version of protoc.
#endif

#include <google/protobuf/port_undef.inc>
#include <google/protobuf/io/coded_stream.h>
#include <google/protobuf/arena.h>
#include <google/protobuf/arenastring.h>
#include <google/protobuf/generated_message_table_driven.h>
#include <google/protobuf/generated_message_util.h>
#include <google/protobuf/metadata_lite.h>
#include <google/protobuf/generated_message_reflection.h>
#include <google/protobuf/message.h>
#include <google/protobuf/repeated_field.h>  // IWYU pragma: export
#include <google/protobuf/extension_set.h>  // IWYU pragma: export
#include <google/protobuf/unknown_field_set.h>
// @@protoc_insertion_point(includes)
#include <google/protobuf/port_def.inc>
#define PROTOBUF_INTERNAL_EXPORT_Fence_2eproto
PROTOBUF_NAMESPACE_OPEN
namespace internal {
class AnyMetadata;
}  // namespace internal
PROTOBUF_NAMESPACE_CLOSE

// Internal implementation detail -- do not use these members.
struct TableStruct_Fence_2eproto {
  static const ::PROTOBUF_NAMESPACE_ID::internal::ParseTableField entries[]
    PROTOBUF_SECTION_VARIABLE(protodesc_cold);
  static const ::PROTOBUF_NAMESPACE_ID::internal::AuxiliaryParseTableField aux[]
    PROTOBUF_SECTION_VARIABLE(protodesc_cold);
  static const ::PROTOBUF_NAMESPACE_ID::internal::ParseTable schema[2]
    PROTOBUF_SECTION_VARIABLE(protodesc_cold);
  static const ::PROTOBUF_NAMESPACE_ID::internal::FieldMetadata field_metadata[];
  static const ::PROTOBUF_NAMESPACE_ID::internal::SerializationTable serialization_table[];
  static const ::PROTOBUF_NAMESPACE_ID::uint32 offsets[];
};
extern const ::PROTOBUF_NAMESPACE_ID::internal::DescriptorTable descriptor_table_Fence_2eproto;
namespace verif {
class FenceReq;
class FenceReqDefaultTypeInternal;
extern FenceReqDefaultTypeInternal _FenceReq_default_instance_;
class FenceResp;
class FenceRespDefaultTypeInternal;
extern FenceRespDefaultTypeInternal _FenceResp_default_instance_;
}  // namespace verif
PROTOBUF_NAMESPACE_OPEN
template<> ::verif::FenceReq* Arena::CreateMaybeMessage<::verif::FenceReq>(Arena*);
template<> ::verif::FenceResp* Arena::CreateMaybeMessage<::verif::FenceResp>(Arena*);
PROTOBUF_NAMESPACE_CLOSE
namespace verif {

// ===================================================================

class FenceReq PROTOBUF_FINAL :
    public ::PROTOBUF_NAMESPACE_ID::Message /* @@protoc_insertion_point(class_definition:verif.FenceReq) */ {
 public:
  inline FenceReq() : FenceReq(nullptr) {}
  virtual ~FenceReq();

  FenceReq(const FenceReq& from);
  FenceReq(FenceReq&& from) noexcept
    : FenceReq() {
    *this = ::std::move(from);
  }

  inline FenceReq& operator=(const FenceReq& from) {
    CopyFrom(from);
    return *this;
  }
  inline FenceReq& operator=(FenceReq&& from) noexcept {
    if (GetArena() == from.GetArena()) {
      if (this != &from) InternalSwap(&from);
    } else {
      CopyFrom(from);
    }
    return *this;
  }

  static const ::PROTOBUF_NAMESPACE_ID::Descriptor* descriptor() {
    return GetDescriptor();
  }
  static const ::PROTOBUF_NAMESPACE_ID::Descriptor* GetDescriptor() {
    return GetMetadataStatic().descriptor;
  }
  static const ::PROTOBUF_NAMESPACE_ID::Reflection* GetReflection() {
    return GetMetadataStatic().reflection;
  }
  static const FenceReq& default_instance();

  static inline const FenceReq* internal_default_instance() {
    return reinterpret_cast<const FenceReq*>(
               &_FenceReq_default_instance_);
  }
  static constexpr int kIndexInFileMessages =
    0;

  friend void swap(FenceReq& a, FenceReq& b) {
    a.Swap(&b);
  }
  inline void Swap(FenceReq* other) {
    if (other == this) return;
    if (GetArena() == other->GetArena()) {
      InternalSwap(other);
    } else {
      ::PROTOBUF_NAMESPACE_ID::internal::GenericSwap(this, other);
    }
  }
  void UnsafeArenaSwap(FenceReq* other) {
    if (other == this) return;
    GOOGLE_DCHECK(GetArena() == other->GetArena());
    InternalSwap(other);
  }

  // implements Message ----------------------------------------------

  inline FenceReq* New() const final {
    return CreateMaybeMessage<FenceReq>(nullptr);
  }

  FenceReq* New(::PROTOBUF_NAMESPACE_ID::Arena* arena) const final {
    return CreateMaybeMessage<FenceReq>(arena);
  }
  void CopyFrom(const ::PROTOBUF_NAMESPACE_ID::Message& from) final;
  void MergeFrom(const ::PROTOBUF_NAMESPACE_ID::Message& from) final;
  void CopyFrom(const FenceReq& from);
  void MergeFrom(const FenceReq& from);
  PROTOBUF_ATTRIBUTE_REINITIALIZES void Clear() final;
  bool IsInitialized() const final;

  size_t ByteSizeLong() const final;
  const char* _InternalParse(const char* ptr, ::PROTOBUF_NAMESPACE_ID::internal::ParseContext* ctx) final;
  ::PROTOBUF_NAMESPACE_ID::uint8* _InternalSerialize(
      ::PROTOBUF_NAMESPACE_ID::uint8* target, ::PROTOBUF_NAMESPACE_ID::io::EpsCopyOutputStream* stream) const final;
  int GetCachedSize() const final { return _cached_size_.Get(); }

  private:
  inline void SharedCtor();
  inline void SharedDtor();
  void SetCachedSize(int size) const final;
  void InternalSwap(FenceReq* other);
  friend class ::PROTOBUF_NAMESPACE_ID::internal::AnyMetadata;
  static ::PROTOBUF_NAMESPACE_ID::StringPiece FullMessageName() {
    return "verif.FenceReq";
  }
  protected:
  explicit FenceReq(::PROTOBUF_NAMESPACE_ID::Arena* arena);
  private:
  static void ArenaDtor(void* object);
  inline void RegisterArenaDtor(::PROTOBUF_NAMESPACE_ID::Arena* arena);
  public:

  ::PROTOBUF_NAMESPACE_ID::Metadata GetMetadata() const final;
  private:
  static ::PROTOBUF_NAMESPACE_ID::Metadata GetMetadataStatic() {
    ::PROTOBUF_NAMESPACE_ID::internal::AssignDescriptors(&::descriptor_table_Fence_2eproto);
    return ::descriptor_table_Fence_2eproto.file_level_metadata[kIndexInFileMessages];
  }

  public:

  // nested types ----------------------------------------------------

  // accessors -------------------------------------------------------

  enum : int {
    kValidFieldNumber = 1,
  };
  // bool valid = 1;
  void clear_valid();
  bool valid() const;
  void set_valid(bool value);
  private:
  bool _internal_valid() const;
  void _internal_set_valid(bool value);
  public:

  // @@protoc_insertion_point(class_scope:verif.FenceReq)
 private:
  class _Internal;

  template <typename T> friend class ::PROTOBUF_NAMESPACE_ID::Arena::InternalHelper;
  typedef void InternalArenaConstructable_;
  typedef void DestructorSkippable_;
  bool valid_;
  mutable ::PROTOBUF_NAMESPACE_ID::internal::CachedSize _cached_size_;
  friend struct ::TableStruct_Fence_2eproto;
};
// -------------------------------------------------------------------

class FenceResp PROTOBUF_FINAL :
    public ::PROTOBUF_NAMESPACE_ID::Message /* @@protoc_insertion_point(class_definition:verif.FenceResp) */ {
 public:
  inline FenceResp() : FenceResp(nullptr) {}
  virtual ~FenceResp();

  FenceResp(const FenceResp& from);
  FenceResp(FenceResp&& from) noexcept
    : FenceResp() {
    *this = ::std::move(from);
  }

  inline FenceResp& operator=(const FenceResp& from) {
    CopyFrom(from);
    return *this;
  }
  inline FenceResp& operator=(FenceResp&& from) noexcept {
    if (GetArena() == from.GetArena()) {
      if (this != &from) InternalSwap(&from);
    } else {
      CopyFrom(from);
    }
    return *this;
  }

  static const ::PROTOBUF_NAMESPACE_ID::Descriptor* descriptor() {
    return GetDescriptor();
  }
  static const ::PROTOBUF_NAMESPACE_ID::Descriptor* GetDescriptor() {
    return GetMetadataStatic().descriptor;
  }
  static const ::PROTOBUF_NAMESPACE_ID::Reflection* GetReflection() {
    return GetMetadataStatic().reflection;
  }
  static const FenceResp& default_instance();

  static inline const FenceResp* internal_default_instance() {
    return reinterpret_cast<const FenceResp*>(
               &_FenceResp_default_instance_);
  }
  static constexpr int kIndexInFileMessages =
    1;

  friend void swap(FenceResp& a, FenceResp& b) {
    a.Swap(&b);
  }
  inline void Swap(FenceResp* other) {
    if (other == this) return;
    if (GetArena() == other->GetArena()) {
      InternalSwap(other);
    } else {
      ::PROTOBUF_NAMESPACE_ID::internal::GenericSwap(this, other);
    }
  }
  void UnsafeArenaSwap(FenceResp* other) {
    if (other == this) return;
    GOOGLE_DCHECK(GetArena() == other->GetArena());
    InternalSwap(other);
  }

  // implements Message ----------------------------------------------

  inline FenceResp* New() const final {
    return CreateMaybeMessage<FenceResp>(nullptr);
  }

  FenceResp* New(::PROTOBUF_NAMESPACE_ID::Arena* arena) const final {
    return CreateMaybeMessage<FenceResp>(arena);
  }
  void CopyFrom(const ::PROTOBUF_NAMESPACE_ID::Message& from) final;
  void MergeFrom(const ::PROTOBUF_NAMESPACE_ID::Message& from) final;
  void CopyFrom(const FenceResp& from);
  void MergeFrom(const FenceResp& from);
  PROTOBUF_ATTRIBUTE_REINITIALIZES void Clear() final;
  bool IsInitialized() const final;

  size_t ByteSizeLong() const final;
  const char* _InternalParse(const char* ptr, ::PROTOBUF_NAMESPACE_ID::internal::ParseContext* ctx) final;
  ::PROTOBUF_NAMESPACE_ID::uint8* _InternalSerialize(
      ::PROTOBUF_NAMESPACE_ID::uint8* target, ::PROTOBUF_NAMESPACE_ID::io::EpsCopyOutputStream* stream) const final;
  int GetCachedSize() const final { return _cached_size_.Get(); }

  private:
  inline void SharedCtor();
  inline void SharedDtor();
  void SetCachedSize(int size) const final;
  void InternalSwap(FenceResp* other);
  friend class ::PROTOBUF_NAMESPACE_ID::internal::AnyMetadata;
  static ::PROTOBUF_NAMESPACE_ID::StringPiece FullMessageName() {
    return "verif.FenceResp";
  }
  protected:
  explicit FenceResp(::PROTOBUF_NAMESPACE_ID::Arena* arena);
  private:
  static void ArenaDtor(void* object);
  inline void RegisterArenaDtor(::PROTOBUF_NAMESPACE_ID::Arena* arena);
  public:

  ::PROTOBUF_NAMESPACE_ID::Metadata GetMetadata() const final;
  private:
  static ::PROTOBUF_NAMESPACE_ID::Metadata GetMetadataStatic() {
    ::PROTOBUF_NAMESPACE_ID::internal::AssignDescriptors(&::descriptor_table_Fence_2eproto);
    return ::descriptor_table_Fence_2eproto.file_level_metadata[kIndexInFileMessages];
  }

  public:

  // nested types ----------------------------------------------------

  // accessors -------------------------------------------------------

  enum : int {
    kCompleteFieldNumber = 1,
  };
  // bool complete = 1;
  void clear_complete();
  bool complete() const;
  void set_complete(bool value);
  private:
  bool _internal_complete() const;
  void _internal_set_complete(bool value);
  public:

  // @@protoc_insertion_point(class_scope:verif.FenceResp)
 private:
  class _Internal;

  template <typename T> friend class ::PROTOBUF_NAMESPACE_ID::Arena::InternalHelper;
  typedef void InternalArenaConstructable_;
  typedef void DestructorSkippable_;
  bool complete_;
  mutable ::PROTOBUF_NAMESPACE_ID::internal::CachedSize _cached_size_;
  friend struct ::TableStruct_Fence_2eproto;
};
// ===================================================================


// ===================================================================

#ifdef __GNUC__
  #pragma GCC diagnostic push
  #pragma GCC diagnostic ignored "-Wstrict-aliasing"
#endif  // __GNUC__
// FenceReq

// bool valid = 1;
inline void FenceReq::clear_valid() {
  valid_ = false;
}
inline bool FenceReq::_internal_valid() const {
  return valid_;
}
inline bool FenceReq::valid() const {
  // @@protoc_insertion_point(field_get:verif.FenceReq.valid)
  return _internal_valid();
}
inline void FenceReq::_internal_set_valid(bool value) {
  
  valid_ = value;
}
inline void FenceReq::set_valid(bool value) {
  _internal_set_valid(value);
  // @@protoc_insertion_point(field_set:verif.FenceReq.valid)
}

// -------------------------------------------------------------------

// FenceResp

// bool complete = 1;
inline void FenceResp::clear_complete() {
  complete_ = false;
}
inline bool FenceResp::_internal_complete() const {
  return complete_;
}
inline bool FenceResp::complete() const {
  // @@protoc_insertion_point(field_get:verif.FenceResp.complete)
  return _internal_complete();
}
inline void FenceResp::_internal_set_complete(bool value) {
  
  complete_ = value;
}
inline void FenceResp::set_complete(bool value) {
  _internal_set_complete(value);
  // @@protoc_insertion_point(field_set:verif.FenceResp.complete)
}

#ifdef __GNUC__
  #pragma GCC diagnostic pop
#endif  // __GNUC__
// -------------------------------------------------------------------


// @@protoc_insertion_point(namespace_scope)

}  // namespace verif

// @@protoc_insertion_point(global_scope)

#include <google/protobuf/port_undef.inc>
#endif  // GOOGLE_PROTOBUF_INCLUDED_GOOGLE_PROTOBUF_INCLUDED_Fence_2eproto
