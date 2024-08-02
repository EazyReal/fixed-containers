#pragma once

#include "fixed_containers/recursive_reflection_fwd.hpp"
#include "fixed_containers/reflection.hpp"
#include "fixed_containers/type_name.hpp"

// reflection strategy concepts


// reflection strategy implementations
namespace fixed_containers::recursive_reflection_detail
{

template <typename S>
    requires(StrategyPrimitive<std::decay_t<S>>)
struct ReflectionHandler<S>
{
    using Type = std::decay_t<S>;
    static constexpr bool reflectable = true;

    template <typename T, typename PreFunction, typename PostFunction>
        requires(std::same_as<std::decay_t<T>, Type>)
    static constexpr void reflect_into(T&& instance,
                                       PreFunction&& pre_fn,
                                       PostFunction&& post_fn,
                                       const PathNameChain& chain = {})
    {
        std::forward<PreFunction>(pre_fn)(chain, std::forward<S>(instance));
        std::forward<PostFunction>(post_fn)(chain, std::forward<S>(instance));
    }
};

// template <typename S>
//     requires(tesla::reflection::reflectable<std::decay_t<S>>)
// struct ReflectionHandler<S>
// {
//     using Type = std::decay_t<S>;
//     static constexpr bool reflectable = true;

//     // This template trick allows us to use a forwarding reference (T&&) while actually constraining
//     // the argument type with the template parameter of the enclosing class.
//     template <typename Function, class T>
//         requires(std::same_as<std::decay_t<T>, Type>)
//     static constexpr void reflect_into(T&& reflected_object,
//                                        Function&& handler,
//                                        const FieldNameChain& parent_chain = {})
//     {
//         for_each_field_recursive_depth_first_order(
//             std::forward<T>(reflected_object),
//             [&]<typename F>(const tesla::reflection::FieldNameChain& chain, F&& field)
//             {
//                 // copy construct to allow extension
//                 FieldNameChain full_chain = parent_chain;
//                 full_chain.insert(full_chain.end(), chain.begin(), chain.end());

//                 // only attempt to recursively reflect with extensible_* if it will not be recursed
//                 // into by the containing reflection::for_each_field_recursive_depth_first_order
//                 if constexpr (!tesla::reflection::reflectable<std::decay_t<F>>)
//                 {
//                     for_each_path_recursive_depth_first_order(
//                         std::forward<F>(field), std::forward<Function>(handler), full_chain);
//                 }
//                 else
//                 {
//                     handler(
//                         ReflectedField<true, decltype(field)>(full_chain, std::forward<F>(field)));
//                 }
//             });
//     }
// };

}  // namespace fixed_containers::recursive_reflection_detail

namespace fixed_containers::recursive_reflection
{

/*
 * This function recursively reflects into a type. (for each path)
 * The function calls corresponding handler to decide how to recursively reflect into the type.
 *
 * See the test for an example of how to do provide custom reflection strategy
 */
template <typename S, typename PreFunction, typename PostFunction>
constexpr void for_each_path_recursive_depth_first_order(
    S&& reflected_object, PreFunction&& pre_fn, PostFunction&& post_fn, const PathNameChain& chain = {})
{
    using Handler =recursive_reflection_detail::ReflectionHandler<std::decay_t<S>>;
    if constexpr (Handler::reflectable)
    {
        Handler::reflect_into(
            std::forward<S>(reflected_object), std::forward<PreFunction>(pre_fn), std::forward<PostFunction>(post_fn), chain);
    }
}

// Will also use NoDefaultStrategy<T> to enable complete override of the reflection strategy
template <typename T>
concept StrategyCovered = detail::ReflectionHandler<std::decay_t<T>>::reflectable;

}  // namespace fixed_containers::recursive_reflection
