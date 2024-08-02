#pragma once

#include "fixed_containers/reflection.hpp"

namespace fixed_containers::recursive_reflection_detail
{

inline constexpr std::size_t MAX_PATH_LENGTH = 16;
using PathNameChain = FixedVector<std::string_view, MAX_PATH_LENGTH>;

template <typename T>
struct ReflectionHandler
{
    static constexpr bool reflectable = false;
};

}  // namespace fixed_containers::recursive_reflection_detail

namespace fixed_containers::recursive_reflection
{

using PathNameChain = recursive_reflection_detail::PathNameChain;

template <typename S, typename PreFunction, typename PostFunction>
constexpr void for_each_path_dfs_helper(S&& reflected_object,
                                        PreFunction&& pre_fn,
                                        PostFunction&& post_fn,
                                        in_out<PathNameChain> chain);

template <typename S, typename PreFunction, typename PostFunction>
constexpr void for_each_path_dfs(S&& reflected_object,
                                 PreFunction&& pre_fn,
                                 PostFunction&& post_fn);

template <typename S, typename PreFunction>
constexpr void for_each_path_dfs(S&& reflected_object, PreFunction&& pre_fn);

}  // namespace fixed_containers::recursive_reflection
