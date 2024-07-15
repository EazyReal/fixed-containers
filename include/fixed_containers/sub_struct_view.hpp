#pragma once

#include "fixed_containers/assert_or_abort.hpp"
#include "fixed_containers/fixed_map.hpp"
#include "fixed_containers/fixed_set.hpp"
#include "fixed_containers/fixed_vector.hpp"
#include "fixed_containers/in_out.hpp"
#include "fixed_containers/iterator_utils.hpp"
#include "fixed_containers/memory.hpp"
#include "fixed_containers/out.hpp"
#include "fixed_containers/random_access_iterator.hpp"
#include "fixed_containers/reflection.hpp"

#include <cstddef>
#include <cstdint>
#include <iterator>
#include <numeric>
#include <optional>
#include <ranges>
#include <string_view>
#include <type_traits>
#include <utility>
#include <concepts>

/**
 * Terminologies
 *
 * A Path:
 * A `PathNameChain` is a sequence of struct field names that leads to a terminal field in the
 * struct, with a caveat that when encountering an iterable, we do not include the index as part of
 * the path, instead we use `data` to represent all the elements in the iterable.
 * `for_each_path_dfs` is a recursive function that iterates over all the paths in the struct.
 *
 * Accessing a field by path:
 * We defer all the indexing to the end of the path,
 * where we can then use the `offset.get_offset(Indices indices)` function to get the offset of the
 * field.
 *
 * Sub struct view:
 * `sub_struct_view_of` create a view of the super struct object in the sub struct object.
 * Currently, `sub_struct_view_of` employs a greedy strategy and updates all the indices for all
 * paths at once. This can be improved for some usecases by using a lazy evaluation strategy, where
 * we only update the indices when we need to. Users will specify a `ContiguousRangeSubStructView`
 * instead of an array to denote the need of lazy evaluation.
 *
 * TODO: `ContiguousRangeSubStructView` currently only supports flat structs.
 * To support partial lazy evaluation, use `PathPropertiesTree` instead of `PathPropertiesMap`
 */

namespace fixed_containers::sub_struct_view_detail
{

inline constexpr std::size_t MAX_NUM_PATHS = 100;
inline constexpr std::size_t MAX_PATH_LENGTH = 16;
inline constexpr std::size_t MAX_DIM = 5;
inline constexpr std::string_view ITERABLE_PATH_NAME = "data[:]";
inline constexpr std::string_view PATH_DELIMITER = ".";

using PathNameChain = FixedVector<std::string_view, MAX_PATH_LENGTH>;

struct Dimension
{
    std::size_t stride{};
    std::size_t size{};
};

template <std::size_t MAXIMUM_SIZE>
using Dimensions = FixedVector<Dimension, MAXIMUM_SIZE>;

template <std::size_t MAXIMUM_SIZE>
using Indices = FixedVector<std::size_t, MAXIMUM_SIZE>;

template <std::size_t MAXIMUM_SIZE>
struct Offset
{
    using Dimensions = Dimensions<MAXIMUM_SIZE>;
    using Indices = Indices<MAXIMUM_SIZE>;

    std::size_t base_offset{};
    Dimensions dimensions{};

    [[nodiscard]] auto get_offset(Indices indices) const
    {
        auto stride_view = dimensions | std::views::transform(&Dimension::stride);
        return std::inner_product(
            std::begin(indices), std::end(indices), std::begin(stride_view), base_offset);
    }

    constexpr bool operator==(const Offset&) const = default;
};

// Recursion Strategy Concepts
template <typename T>
concept Iterable = std::ranges::sized_range<T> && std::ranges::contiguous_range<T>;

template <typename T>
concept NonTerminal = reflection::Reflectable<T> || Iterable<T>;

template <typename T>
concept Terminal = !NonTerminal<T>;

template <typename T>
concept Branch = reflection::Reflectable<T> && !Iterable<T>;

template <typename S, typename PreFunction, typename PostFunction>
    requires(Iterable<std::decay_t<S>>)
constexpr void for_each_path_dfs_helper(S&& instance,
                                        PreFunction&& pre_fn,
                                        PostFunction&& post_fn,
                                        fixed_containers::in_out<PathNameChain> chain);
template <typename S, typename PreFunction, typename PostFunction>
    requires(Branch<std::decay_t<S>>)
constexpr void for_each_path_dfs_helper(S&& instance,
                                        PreFunction&& pre_fn,
                                        PostFunction&& post_fn,
                                        fixed_containers::in_out<PathNameChain> chain);
template <typename S, typename PreFunction, typename PostFunction>
    requires(Terminal<std::decay_t<S>>)
constexpr void for_each_path_dfs_helper(S&& instance,
                                        PreFunction&& pre_fn,
                                        PostFunction&& post_fn,
                                        fixed_containers::in_out<PathNameChain> chain);
template <typename S, typename PreFunction, typename PostFunction>
constexpr void for_each_path_dfs_helper(S&& instance,
                                        PreFunction&& pre_fn,
                                        PostFunction&& post_fn,
                                        fixed_containers::in_out<PathNameChain> chain);

template <typename S, typename PreFunction, typename PostFunction>
    requires(Iterable<std::decay_t<S>>)
constexpr void for_each_path_dfs_helper(S&& instance,
                                        PreFunction&& pre_fn,
                                        PostFunction&& post_fn,
                                        fixed_containers::in_out<PathNameChain> chain)
{
    std::forward<PreFunction>(pre_fn)(std::as_const(*chain), std::forward<S>(instance));
    chain->push_back(ITERABLE_PATH_NAME);
    for_each_path_dfs_helper(*std::forward<S>(instance).data(), std::forward<PreFunction>(pre_fn), std::forward<PostFunction>(post_fn), fixed_containers::in_out{*chain});
    chain->pop_back();
    std::forward<PostFunction>(post_fn)(std::as_const(*chain), std::forward<S>(instance));
}

template <typename S, typename PreFunction, typename PostFunction>
    requires(Branch<std::decay_t<S>>)
constexpr void for_each_path_dfs_helper(S&& instance,
                                        PreFunction&& pre_fn,
                                        PostFunction&& post_fn,
                                        fixed_containers::in_out<PathNameChain> chain)
{
    std::forward<PreFunction>(pre_fn)(std::as_const(*chain), std::forward<S>(instance));
    reflection::for_each_field(
        std::forward<S>(instance),
        [&pre_fn, &post_fn, &chain]<typename T>(const std::string_view& name, T& field)
        {
            chain->push_back(name);
            for_each_path_dfs_helper(field, std::forward<PreFunction>(pre_fn), std::forward<PostFunction>(post_fn), fixed_containers::in_out{*chain});
            chain->pop_back();
        });
    std::forward<PostFunction>(post_fn)(std::as_const(*chain), std::forward<S>(instance));
}

template <typename S, typename PreFunction, typename PostFunction>
    requires(Terminal<std::decay_t<S>>)
constexpr void for_each_path_dfs_helper(S&& instance,
                                        PreFunction&& pre_fn,
                                        PostFunction&& post_fn,
                                        fixed_containers::in_out<PathNameChain> chain)
{
    std::forward<PreFunction>(pre_fn)(std::as_const(*chain), std::forward<S>(instance));
    std::forward<PostFunction>(post_fn)(std::as_const(*chain), std::forward<S>(instance));
}

template <typename S, typename PreFunction, typename PostFunction>
constexpr void for_each_path_dfs_helper(S&& /*instance*/,
                                        PreFunction&& /*pre_fn*/,
                                        PostFunction&& /*post_fn*/,
                                        fixed_containers::in_out<PathNameChain> /*chain*/)
{
    static_assert(std::is_same_v<S, void>, "Unreachable Fallback");
}

// template function that expands to MAX_DIM nested loops for iterating all indices
template <std::size_t DIM, std::size_t MAXIMUM_SIZE>
constexpr void for_each_index_helper(const Offset<MAXIMUM_SIZE>& offset,
                                     auto&& func,
                                     Indices<MAXIMUM_SIZE>& indices)
{
    // DIM == std::size(indices)
    if (DIM == std::size(offset.dimensions))
    {
        func(indices);
        return;
    }

    for (std::size_t i = 0; i < offset.dimensions[DIM].size; ++i)
    {
        indices.push_back(i);
        if constexpr (DIM < MAXIMUM_SIZE)
        {
            for_each_index_helper<DIM + 1>(offset, func, indices);
        }
        indices.pop_back();
    }
}

// runtime version of offsetof in <cstddef>
template <typename Instance, typename Field>
std::size_t get_pointer_distance(Instance&& instance, Field&& field)
{
    const std::byte* instance_ptr = memory::addressof_as_const_byte_ptr(instance);
    const std::byte* field_ptr = memory::addressof_as_const_byte_ptr(field);
    assert_or_abort(instance_ptr <= field_ptr);
    return static_cast<std::size_t>(std::distance(instance_ptr, field_ptr));
}

}  // namespace fixed_containers::sub_struct_view_detail

namespace fixed_containers::sub_struct_view
{

inline constexpr std::size_t MAX_NUM_PATHS = sub_struct_view_detail::MAX_NUM_PATHS;
using PathNameChain = sub_struct_view_detail::PathNameChain;
using Dimension = sub_struct_view_detail::Dimension;
using Dimensions = sub_struct_view_detail::Dimensions<sub_struct_view_detail::MAX_DIM>;
using Indices = sub_struct_view_detail::Indices<sub_struct_view_detail::MAX_DIM>;
using Offset = sub_struct_view_detail::Offset<sub_struct_view_detail::MAX_DIM>;

enum class StructTreeNodeType
{
    BRANCH,
    TERMINAL,
    ITERABLE,
};

struct PathProperties
{
    StructTreeNodeType type{};
    Offset offset{};

    constexpr bool operator==(const PathProperties&) const = default;
};

// This function iterates over all paths of a given struct and calls a pre and post function for
// each field.
// We do not perfect forward the instance because we want it to be an lvalue.
template <typename S, typename PreFunction, typename PostFunction>
    requires(reflection::Reflectable<std::decay_t<S>>)
constexpr void for_each_path_dfs(S&& instance, PreFunction&& pre_fn, PostFunction&& post_fn)
{
    PathNameChain chain{};
    sub_struct_view_detail::for_each_path_dfs_helper(
        instance, std::forward<PreFunction>(pre_fn), std::forward<PostFunction>(post_fn), fixed_containers::in_out{chain});
}

template <typename S, typename Function>
    requires(reflection::Reflectable<std::decay_t<S>>)
constexpr void for_each_path_dfs(S&& instance, Function&& func)
{
    PathNameChain chain{};
    sub_struct_view_detail::for_each_path_dfs_helper(
        instance, std::forward<Function>(func), [](const auto&, auto&){}, fixed_containers::in_out{chain});
}

template <typename S>
constexpr std::size_t path_count_of()
{
    std::size_t count = 0;
    for_each_path_dfs(S{}, [&count](const auto&, auto&) { ++count; });
    return count;
}

template <std::size_t MAXIMUM_SIZE=MAX_NUM_PATHS>
using PathPropertiesMap = FixedMap<PathNameChain, PathProperties, MAXIMUM_SIZE>;
template <std::size_t MAXIMUM_SIZE=MAX_NUM_PATHS>
using PathSet = FixedSet<PathNameChain, MAXIMUM_SIZE>;

inline PathNameChain path_from_string(const std::string_view& path_name_chain_string)
{
    auto view_of_string_view =
        path_name_chain_string | std::views::split(sub_struct_view_detail::PATH_DELIMITER) |
        std::views::transform(
            [](auto&& name)
            { return std::string_view(std::ranges::begin(name), std::ranges::size(name)); });
    return PathNameChain(std::ranges::begin(view_of_string_view),
                         std::ranges::end(view_of_string_view));
}

template <typename S, std::size_t MAXIMUM_SIZE=MAX_NUM_PATHS>
auto extract_paths_of(const S& instance = {})
{
    PathSet<MAXIMUM_SIZE> paths{};

    for_each_path_dfs(
        instance,
        [&]<typename F>(const PathNameChain& chain, const F& /*field*/) { paths.insert(chain); });
    return paths;
}

template <typename S, std::size_t MAXIMUM_SIZE_IN=MAX_NUM_PATHS, std::size_t MAXIMUM_SIZE_OUT=MAX_NUM_PATHS>
auto extract_path_properties_of_filtered(
    const S& instance, const std::optional<PathSet<MAXIMUM_SIZE_IN>>& registered_set = std::nullopt)
{
    PathPropertiesMap<MAXIMUM_SIZE_OUT> paths{};
    Dimensions dimensions{};

    for_each_path_dfs(
        instance,
        [&]<typename F>(const PathNameChain& chain, const F& field)
        {
            if (registered_set.has_value() && !registered_set.value().contains(chain))
            {
                return;
            }
            if constexpr (sub_struct_view_detail::Terminal<F>)
            {
                auto [_, was_inserted] = paths.try_emplace(
                    chain,
                    PathProperties{
                        .type = StructTreeNodeType::TERMINAL,
                        .offset = {.base_offset = sub_struct_view_detail::get_pointer_distance(
                                       instance, field),
                                   .dimensions = dimensions},
                    });
                assert_or_abort(was_inserted);
            }
            else if constexpr (sub_struct_view_detail::Iterable<F>)
            {
                dimensions.push_back({
                    .stride = sizeof(std::ranges::range_value_t<F>),
                    .size = std::size(field),
                });
                auto [_, was_inserted] = paths.try_emplace(
                    chain,
                    PathProperties{
                        .type = StructTreeNodeType::ITERABLE,
                        .offset = {.base_offset = sub_struct_view_detail::get_pointer_distance(
                                       instance, field),
                                   .dimensions = dimensions},
                    });
                assert_or_abort(was_inserted);
            }
            else if constexpr (sub_struct_view_detail::Branch<F>)
            {
                // Branch nodes will not be part of path properties.
                // They can be used naturally inside of the sub struct.
            }
        },
        [&]<typename F>(const PathNameChain& /*chain*/, const F& /*field*/)
        {
            if constexpr (sub_struct_view_detail::Iterable<F>)
            {
                dimensions.pop_back();
            }
        });
    return paths;
}

template <typename S, std::size_t MAXIMUM_SIZE=MAX_NUM_PATHS>
auto extract_path_properties_of(const S& instance = {})
{
    return extract_path_properties_of_filtered<S, 0, MAXIMUM_SIZE>(instance, std::nullopt);
}

void for_each_index(const Offset& offset, auto&& func)
{
    Indices indices;
    for_each_index_helper<0>(offset, func, indices);
}

// This is the user facing interface for a view of a struct
template <std::size_t MAXIMUM_SIZE = MAX_NUM_PATHS>
class StructView
{
public:
    StructView() = default;

    template <typename S>
    StructView(const S& instance = {}):
    path_properties_(extract_path_properties_of(instance))
    {
    }

    template <typename SuperStruct, typename SubStruct>
    StructView(const SuperStruct& super_struct={}, const SubStruct& sub_struct={})
    {
        auto sub_struct_paths = extract_paths_of(sub_struct);
        path_properties_ = extract_path_properties_of_filtered(super_struct, std::optional{sub_struct_paths});
    }

    template <typename S>
    void add_path(const S& instance, const PathNameChain& path) {
        auto path_properties_map = extract_path_properties_of_filtered<S, PathSet<1>, MAXIMUM_SIZE>(instance, {path});
        auto [_, was_inserted] = path_properties_.try_emplace(path, path_properties_map.at(path));
        assert_or_abort(was_inserted);
    }

    template <typename S>
    void add_path(const PathNameChain& path) {
        add_path(S{}, path);
    }

    template <typename S, typename PathSet>
    void add_paths(const S& instance, const PathSet& paths) {
        auto path_properties_map = extract_path_properties_of_filtered(instance, paths);
        for (const auto& [path, path_properties] : path_properties_map) {
            auto [_, was_inserted] = path_properties_.try_emplace(path, path_properties);
            assert_or_abort(was_inserted);
        }
    }

    template <typename S, typename PathSet>
    void add_paths(const PathSet& paths) {
        add_paths(S{}, paths);
    }

    [[nodiscard]] PathProperties at(const PathNameChain& path) const {
        return path_properties_.at(path);
    }

    const PathPropertiesMap<MAXIMUM_SIZE>& get_path_map_ref() const {
        return path_properties_;
    }

    const void* get_field(const void* instance, const PathNameChain& path, const Indices& indices) {
        auto path_properties = path_properties_.at(path);
        return static_cast<const void*>(std::next(static_cast<const std::byte*>(instance), static_cast<std::ptrdiff_t>(path_properties.offset.get_offset(indices))));
    }

    template <typename Function>
    static void for_each_field(const StructView& struct_view, const void* base_pointer, Function&& func)
    requires(
        std::invocable<Function, const PathNameChain&, const Indices&, const void*>)
    {
        for (const auto& [path, path_properties] : struct_view.get_path_map_ref())
        {
            for_each_index(
                path_properties.offset,
                [&](const auto& indices)
                {
                    const void* field_ptr = static_cast<const void*>(std::next(static_cast<const std::byte*>(base_pointer), static_cast<std::ptrdiff_t>(path_properties.offset.get_offset(indices))));
                    std::forward<Function>(func)(path, indices, field_ptr);
                });
        }
    }
private:
// now we are storing the `StructView` in a map
// in the future when we may want to store in a tree (a path on a tree will be the PathNameChain) to support lazy evaluation
    PathPropertiesMap<MAXIMUM_SIZE> path_properties_;
};

template <std::size_t SUPER_STRUCT_VIEW_MAXIMUM_SIZE=MAX_NUM_PATHS, std::size_t SUB_STRUCT_VIEW_MAXIMUM_SIZE=MAX_NUM_PATHS>
void sub_struct_view_of(const std::byte* base_super_struct_pointer,
                        const StructView<SUPER_STRUCT_VIEW_MAXIMUM_SIZE>& super_struct_view,
                        std::byte* base_sub_struct_pointer,
                        const StructView<SUB_STRUCT_VIEW_MAXIMUM_SIZE>& sub_struct_view)
requires (SUPER_STRUCT_VIEW_MAXIMUM_SIZE >= SUB_STRUCT_VIEW_MAXIMUM_SIZE)
{
    for (const auto& [path, path_properties] : sub_struct_view.get_path_map_ref())
    {
        Offset super_struct_offset = super_struct_view.at(path).offset;
        Offset sub_struct_offset = sub_struct_view.at(path).offset;

        for_each_index(
            sub_struct_offset,
            [&](const auto& indices)
            {
                const std::byte* super_struct_field_ptr =
                    std::next(base_super_struct_pointer,
                              static_cast<std::ptrdiff_t>(super_struct_offset.get_offset(indices)));
                std::byte* sub_struct_field_ptr =
                    std::next(base_sub_struct_pointer,
                              static_cast<std::ptrdiff_t>(sub_struct_offset.get_offset(indices)));
                *reinterpret_cast<std::uintptr_t*>(sub_struct_field_ptr) =
                    reinterpret_cast<std::uintptr_t>(super_struct_field_ptr);
            }
        );
    }
}

template <typename SuperStruct, std::size_t SUPER_STRUCT_VIEW_MAXIMUM_SIZE=MAX_NUM_PATHS, typename SubStruct, std::size_t SUB_STRUCT_VIEW_MAXIMUM_SIZE=MAX_NUM_PATHS>
void sub_struct_view_of(const SuperStruct& super_struct,
                        const StructView<SUPER_STRUCT_VIEW_MAXIMUM_SIZE>& super_struct_view,
                        out<SubStruct> out_sub_struct,
                        const StructView<SUB_STRUCT_VIEW_MAXIMUM_SIZE>& sub_struct_view)
requires (SUPER_STRUCT_VIEW_MAXIMUM_SIZE >= SUB_STRUCT_VIEW_MAXIMUM_SIZE)
{
    const std::byte* base_super_struct_pointer = memory::addressof_as_const_byte_ptr(super_struct);
    std::byte* base_sub_struct_pointer = memory::addressof_as_mutable_byte_ptr(*out_sub_struct);

    return sub_struct_view_of(base_super_struct_pointer,
                              super_struct_view,
                              base_sub_struct_pointer,
                              sub_struct_view);
}

template <typename SubStruct, std::size_t MAXIMUM_SIZE = MAX_NUM_PATHS>
class ContiguousRangeSubStructView
{
    struct AccessingInfo
    {
        StructView<MAXIMUM_SIZE> sub_struct_view{};
        StructView<MAXIMUM_SIZE> super_struct_view{};
        std::byte* base_array_super_struct_ptr{};
        std::size_t stride{};
        std::size_t size{};
    };

    static SubStruct create_view_at_offset(const AccessingInfo& accessing_info,
                                           const std::size_t index)
    {
        assert_or_abort(index < accessing_info.size);
        SubStruct instance{};
        std::byte* base_of_ith_entry =
            std::next(accessing_info.base_array_super_struct_ptr,
                      static_cast<std::ptrdiff_t>(index * accessing_info.stride));
        sub_struct_view::sub_struct_view_of(base_of_ith_entry,
                                            accessing_info.super_struct_view,
                                            memory::addressof_as_mutable_byte_ptr(instance),
                                            accessing_info.sub_struct_view);
        return instance;
    }

    using ReferenceType = SubStruct;

    class ReferenceProvider
    {
    private:
        const AccessingInfo* accessing_info_;
        std::size_t current_index_;

    public:
        constexpr ReferenceProvider() noexcept
          : ReferenceProvider{nullptr, 0}
        {
        }

        constexpr ReferenceProvider(const AccessingInfo& accessing_info,
                                    const std::size_t& current_index) noexcept
          : accessing_info_{&accessing_info}
          , current_index_{current_index}
        {
        }

        constexpr void advance(const std::size_t n) noexcept { current_index_ += n; }
        constexpr void recede(const std::size_t n) noexcept { current_index_ -= n; }

        [[nodiscard]] constexpr ReferenceType get() const noexcept
        {
            return create_view_at_offset(*accessing_info_, current_index_);
        }

        constexpr bool operator==(const ReferenceProvider& other) const noexcept
        {
            assert_or_abort(accessing_info_ == other.accessing_info_);
            return current_index_ == other.current_index_;
        }
        constexpr auto operator<=>(const ReferenceProvider& other) const noexcept
        {
            assert_or_abort(accessing_info_ == other.accessing_info_);
            return current_index_ <=> other.current_index_;
        }

        constexpr std::ptrdiff_t operator-(const ReferenceProvider& other) const noexcept
        {
            assert_or_abort(accessing_info_ == other.accessing_info_);
            return static_cast<std::ptrdiff_t>(current_index_ - other.current_index_);
        }
    };

    using IteratorType = RandomAccessIterator<ReferenceProvider,
                                              ReferenceProvider,
                                              IteratorConstness::CONSTANT_ITERATOR,
                                              IteratorDirection::FORWARD>;

public:
    using const_reference = ReferenceType;
    using const_iterator = IteratorType;

private:
    AccessingInfo accessing_info_;

public:
    ContiguousRangeSubStructView()
      : accessing_info_{}
    {
    }

    template <typename SuperStructContainer>
    ContiguousRangeSubStructView(SuperStructContainer& super_struct_container)
      : accessing_info_{
            .sub_struct_view = {},
            .super_struct_view = {},
            .base_array_super_struct_ptr =
                memory::addressof_as_mutable_byte_ptr(*super_struct_container.data()),
            .stride = {},
            .size = super_struct_container.size(),
        }
    {
        using SuperStruct = typename SuperStructContainer::value_type;
        SubStruct sub_struct_instance{};
        accessing_info_.sub_struct_view = StructView(sub_struct_instance);
        accessing_info_.super_struct_view = StructView(SuperStruct{}, sub_struct_instance);
        accessing_info_.stride = sizeof(SuperStruct);
    }

    [[nodiscard]] const_reference at(const std::size_t index) const
    {
        return create_view_at_offset(accessing_info_, index);
    }

    [[nodiscard]] std::size_t size() const { return accessing_info_.size; }

    constexpr const_iterator begin() noexcept { return cbegin(); }
    [[nodiscard]] constexpr const_iterator begin() const noexcept { return cbegin(); }
    [[nodiscard]] constexpr const_iterator cbegin() const noexcept
    {
        return create_const_iterator(0);
    }

    constexpr const_iterator end() noexcept { return cend(); }
    [[nodiscard]] constexpr const_iterator end() const noexcept { return cend(); }
    [[nodiscard]] constexpr const_iterator cend() const noexcept
    {
        return create_const_iterator(accessing_info_.size);
    }

private:
    [[nodiscard]] constexpr const_iterator create_const_iterator(
        const std::size_t offset_from_start) const noexcept
    {
        return const_iterator{ReferenceProvider{accessing_info_, offset_from_start}};
    }
};

}  // namespace fixed_containers::sub_struct_view
