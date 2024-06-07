#ifndef __GAIN_KEYTREE_HEADER__
#define __GAIN_KEYTREE_HEADER__

#include "basic.h"
#include "cast.h"
#include "holder.h"

#include <cassert>
#include <functional>
#include <ranges>
#include <tuple>
#include <type_traits>
#include <utility>

namespace gain
{
namespace detail::ktree
{

template< typename T > using Key = std::tuple_element_t< 0, T >;

template< typename T > constexpr auto& key( T& value ) requires requires { std::get<0>(value); }
{
	return std::get<0>(value);
}

template< typename T > constexpr auto& key( T& value ) requires requires { value.template get<0>(); }
{
	return value.template get<0>();
}

namespace trait
{

template< typename T > concept value = requires( T value )
{
	key(value);
};

} // namespace trait

template< typename T, typename Key > concept path = requires( T path )
{
	requires std::is_convertible_v< std::iter_value_t< decltype( path.begin() ) >, Key >; // TODO: use std::ranges::range_value_t
};

template< typename T, typename Key > concept comparison = std::is_default_constructible_v<T> && std::is_invocable_r_v< bool, T, const Key, const Key >;

template< typename Value > struct Node;

template< typename Value, comparison< Key<Value> > Comparison = std::less<>, typename Allocator = std::allocator<void> > class KeyTree;

template< typename Value, typename Compare, typename Allocator >
using Nodes = std::vector< KeyTree< Value, Compare, Allocator >, typename std::allocator_traits<Allocator>::template rebind_alloc< KeyTree< Value, Compare, Allocator > > >;

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
class KeyTree : private Nodes< Value, Comparison, Allocator >, public Value
{
	using Nodes = Nodes< Value, Comparison, Allocator >;
	static_assert( noexcept( Nodes( std::declval<Nodes>() ) ) );

public:
	using Nodes::back;
	using Nodes::begin;
	using Nodes::clear;
	using Nodes::empty;
	using Nodes::front;
	using Nodes::end;
	using Nodes::erase;
	using Nodes::rbegin;
	using Nodes::rend;
	using Nodes::size;

	using allocator_type = Allocator;
	using key_compare = Comparison;
	using key_type = Key<Value>;
	using value_type = Value;

	using value_type::operator=;

	using typename Nodes::pointer;
	using typename Nodes::const_pointer;
	using typename Nodes::size_type;
	using typename Nodes::difference_type;
	using typename Nodes::reference;
	using typename Nodes::const_reference;
	using typename Nodes::iterator;
	using typename Nodes::const_iterator;
	using typename Nodes::reverse_iterator;
	using typename Nodes::const_reverse_iterator;

	template< typename... Ts > requires std::is_default_constructible_v<allocator_type> && std::is_constructible_v< value_type, Ts... >
	KeyTree( Ts&&... ts ) : value_type( std::forward<Ts>(ts)... )
	{
	}

	template< typename... Ts > requires std::is_constructible_v< value_type, Ts... >
	KeyTree( const allocator_type& alloc, Ts&&... ts ) : value_type( std::forward<Ts>(ts)... ), allocator(alloc)
	{
	}

	KeyTree( allocator_type&& alloc ) requires std::is_default_constructible_v<value_type>
	: allocator( std::forward<allocator_type>(alloc) )
	{
	}

	KeyTree( const KeyTree& other ) noexcept : Nodes(other), value_type(other), allocator(other.allocator), prnt( other.prnt )
	{
		for( auto& node : *this )
		{
			node.prnt = this;
		}
	}

	KeyTree( KeyTree& other ) noexcept : KeyTree( std::as_const(other) ) // TODO: work around the xcode libc++ issue with vector::operator=
	{
	}

	KeyTree( KeyTree&& other ) noexcept : Nodes( std::forward<Nodes>(other) ), value_type( std::forward<value_type>(other) ), allocator( std::move( other.allocator ) ), prnt( other.prnt )
	{
		for( auto& node : *this )
		{
			node.prnt = this;
		}

		other.prnt = nullptr;
	}

	KeyTree& operator=( KeyTree&& other ) noexcept
	{
		Nodes::operator=( std::forward<Nodes>(other) );
		for( auto& node : *this )
		{
			node.prnt = this;
		}

		value_type::operator=( std::forward<value_type>(other) );
		allocator = std::move( other.allocator );
		prnt = std::exchange( other.prnt, nullptr );

		return *this;
	}

	KeyTree& operator=( const KeyTree& other ) noexcept
	{
		Nodes::operator=(other);
		for( auto& node : *this )
		{
			node.prnt = this;
		}

		value_type::operator=(other);
		allocator = other.allocator;
		prnt = other.prnt;

		return *this;
	}

	iterator find( const std::convertible_to<key_type> auto& );
	const_iterator find( const std::convertible_to<key_type> auto& ) const;

	pointer find( const path<key_type> auto& );
	const_pointer find( const path<key_type> auto& ) const;

	iterator lower_bound( const std::convertible_to<key_type> auto& );
	const_iterator lower_bound( const std::convertible_to<key_type> auto& ) const;

	iterator upper_bound( const std::convertible_to<key_type> auto& key );
	const_iterator upper_bound( const std::convertible_to<key_type> auto& ) const;

	iterator emplace( std::convertible_to<key_type> auto&&, auto&&... );
	iterator emplace( const_iterator, std::convertible_to<key_type> auto&&, auto&&... );

	void erase( const key_type& );

	template< std::convertible_to<key_type> Key > reference operator[]( Key&& ) requires (std::is_constructible_v< Value, Key > || std::is_default_constructible_v<Value>);
	template< path<key_type> Path > reference operator[]( const Path& ) requires (std::is_constructible_v< Value, std::ranges::range_reference_t<Path> > || std::is_default_constructible_v<Value>);

	auto trace( path<key_type> auto&& );
	auto trace( path<key_type> auto&& ) const;

	auto traverse();
	auto traverse() const;

	auto traverse( std::size_t depth );
	auto traverse( std::size_t depth ) const;

	template< std::size_t DEPTH > auto traverse( std::integral_constant< std::size_t, DEPTH > );
	template< std::size_t DEPTH > auto traverse( std::integral_constant< std::size_t, DEPTH > ) const;

	pointer parent() { return prnt; }
	const_pointer parent() const { return prnt; }

private:
	static auto do_find( auto& tree, const path<key_type> auto& );
	auto do_emplace( std::convertible_to<key_type> auto&&, auto&&... );
	auto do_emplace( const_iterator, std::convertible_to<key_type> auto&&, auto&&... );

	struct : key_compare
	{
		using key_compare::key_compare;

		bool operator()( const value_type& left, const value_type& right )
		{
			return key_compare::operator()( key(left), key(right) );
		}

		bool operator()( const value_type& node, const key_type& k )
		{
			return key_compare::operator()( key(node), k );
		}

		bool operator()( const key_type& k, const value_type& node )
		{
			return key_compare::operator()( k, key(node) );
		}
	}
	comparison;

	allocator_type allocator;
	pointer prnt = nullptr;
};

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
template< typename Tree > auto KeyTree< Value, Comparison, Allocator >::do_find( Tree& tree, const path<key_type> auto& path )
{
	auto node = &tree;

	for( auto key = path.begin(); node && key != path.end(); ++key )
	{
		const auto it = node->find( *key );
		node = it != node->end() ? &*it : nullptr;
	}

	return node;
}

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
template< std::convertible_to< Key<Value> > K, typename... Args > auto KeyTree< Value, Comparison, Allocator >::do_emplace( const_iterator hint, K&& k, Args&&... args )
{
	iterator it;

	if constexpr( std::is_constructible_v< Value, K, Args... > )
	{
		it = Nodes::emplace( hint, allocator, std::forward<K>(k), std::forward<Args>(args)... );
	}
	else if constexpr( std::is_constructible_v< Value, std::piecewise_construct_t, std::tuple< K&& >, std::tuple< Args&&... > > )
	{
		it = Nodes::emplace( hint, allocator, std::piecewise_construct, forward_as_tuple<K>(k), forward_as_tuple< Args... >( args... ) );
	}
	else if constexpr( std::is_default_constructible_v<Value> && !sizeof...(Args) )
	{
		it = Nodes::emplace( hint, allocator );
		key( *it ) = std::forward<K>(k);
	}
	else
		assert(false); // TODO: set a constraint

	it->prnt = this;

	return it;
};

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
template< std::convertible_to< Key<Value> > K, typename... Args > auto KeyTree< Value, Comparison, Allocator >::do_emplace( K&& k, Args&&... args )
{
	auto it = std::lower_bound( Nodes::begin(), Nodes::end(), k, comparison );

	if( it == Nodes::end() || key( *it ) < k || k < key( *it ) )
	{
		it = do_emplace( it, std::forward<K>(k), std::forward<Args>(args)... );
	}

	return it;
};

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
auto KeyTree< Value, Comparison, Allocator >::find( const std::convertible_to<key_type> auto& k ) -> iterator
{
	auto it = std::lower_bound( Nodes::begin(), Nodes::end(), k, comparison );
	return it != Nodes::end() && key( *it ) == k ? it : Nodes::end();
}

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
auto KeyTree< Value, Comparison, Allocator >::find( const std::convertible_to<key_type> auto& k ) const -> const_iterator
{
	auto it = std::lower_bound( Nodes::begin(), Nodes::end(), k, comparison );
	return it != Nodes::end() && key( *it ) == k ? it : Nodes::end();
}

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
auto KeyTree< Value, Comparison, Allocator >::find( const path<key_type> auto& path ) -> pointer
{
	return do_find( *this, path );
}

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
auto KeyTree< Value, Comparison, Allocator >::find( const path<key_type> auto& path ) const -> const_pointer
{
	return do_find( *this, path );
}

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
auto KeyTree< Value, Comparison, Allocator >::lower_bound( const std::convertible_to<key_type> auto& key ) -> iterator
{
	return std::lower_bound( Nodes::begin(), Nodes::end(), key, comparison );
}

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
auto KeyTree< Value, Comparison, Allocator >::lower_bound( const std::convertible_to<key_type> auto& key ) const -> const_iterator
{
	return std::lower_bound( Nodes::begin(), Nodes::end(), key, comparison );
}

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
auto KeyTree< Value, Comparison, Allocator >::upper_bound( const std::convertible_to<key_type> auto& key ) -> iterator
{
	return std::upper_bound( Nodes::begin(), Nodes::end(), key, comparison );
}

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
auto KeyTree< Value, Comparison, Allocator >::upper_bound( const std::convertible_to<key_type> auto& key ) const -> const_iterator
{
	return std::upper_bound( Nodes::begin(), Nodes::end(), key, comparison );
}

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
template< std::convertible_to< Key<Value> > Key, typename... Vs > auto KeyTree< Value, Comparison, Allocator >::emplace( Key&& key, Vs&&... vs ) -> iterator
{
	return do_emplace( std::forward<Key>(key), std::forward<Vs>(vs)... );
};

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
template< std::convertible_to< Key<Value> > Key, typename... Vs > auto KeyTree< Value, Comparison, Allocator >::emplace( const_iterator hint, Key&& key, Vs&&... vs ) -> iterator
{
	return do_emplace( hint, std::forward<Key>(key), std::forward<Vs>(vs)... );
}

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
void KeyTree< Value, Comparison, Allocator >::erase( const key_type& k )
{
	const auto it = std::lower_bound( Nodes::begin(), Nodes::end(), k, comparison );
	if( it != Nodes::end() && key( *it ) == k )
	{
		Nodes::erase(it);
	}
}

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
template< std::convertible_to< Key<Value> > Key > auto KeyTree< Value, Comparison, Allocator >::operator[]( Key&& key ) -> reference requires (std::is_constructible_v< Value, Key > || std::is_default_constructible_v<Value>)
{
	return *do_emplace( std::forward<Key>(key) );
}

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
template< path< Key<Value> > Path > auto KeyTree< Value, Comparison, Allocator >::operator[]( const Path& path ) -> reference requires (std::is_constructible_v< Value, std::ranges::range_reference_t<Path> > || std::is_default_constructible_v<Value>)
{
	KeyTree* tree = this;

	for( auto& key : path )
	{
		tree = &tree->operator[]( std::as_const(key) );
	}

	return *tree;
}

namespace trace
{

template< typename Key > class Sentinel
{
	template< typename, typename > friend class Iterator;

public:
	constexpr Sentinel( Key k ) : key( std::move(k) )
	{
	}

	constexpr bool operator==( const Sentinel& other ) const
	{
		return key == other.key;
	}

private:
	Key key;
};

template< typename Tree, typename Key > class Iterator : Sentinel<Key>
{
public:
	using value_type = typename Tree::value_type;
	using difference_type = std::ptrdiff_t;
	using pointer = Tree*;
	using reference = Tree&;
	using iterator_category = std::input_iterator_tag;

	constexpr Iterator( Tree& tree, Key key ) : node( &tree ), Sentinel<Key>( std::move(key) )
	{
	}

	template< std::sentinel_for<Key> T > constexpr bool operator==( const Sentinel<T>& sentinel ) const
	{
		return key == sentinel.key || !node;
	}

	bool operator==( const Iterator& other ) const
	{
		return key == other.key;
	}

	constexpr Iterator& operator++()
	{
		const auto it = node->find( *key++ );
		node = it != node->end() ? &*it : nullptr;

		return *this;
	}

	constexpr reference operator*() const
	{
		return *node;
	}

private:
	Tree* node = nullptr;
	using Sentinel<Key>::key;
};

template< typename Key > Sentinel(Key) -> Sentinel<Key>;
template< typename Tree, typename Key > Iterator( Tree*,  Key ) -> Iterator< Tree, Key >;

template< typename Tree, std::ranges::range Path > class View : std::ranges::view_interface< View< Tree, Path > >
{
public:
	constexpr View( Tree& tree, Path&& path ) : tree(tree), path( std::forward<Path>(path) )
	{
	}

	constexpr View( const View& ) = default;
	constexpr View& operator=( const View& ) = default;

	constexpr View( View&& ) = default;
	constexpr View& operator=( View&& ) = default;

	constexpr auto begin() const
	{
		return Iterator{ tree, path->begin() };
	}

	constexpr auto end() const
	{
		return Sentinel{ path->end() };
	}

	constexpr bool empty() const { return false; }

private:
	Tree& tree;
	Holder<Path> path;
};

template< typename Tree, std::ranges::range Path > View( Tree&, Path&& ) -> View< Tree, Path >;

} // namespace trace
namespace traverse
{

struct Sentinel
{
};

template< typename Item, typename Capacity > struct Trace;

template< typename Item > struct Trace< Item, std::size_t > : std::vector<Item>
{
	using Base = std::vector<Item>;

	constexpr Trace( std::size_t capacity )
	{
		Base::reserve(capacity);
	}

	constexpr bool full() const
	{
		return Base::size() == Base::capacity();
	}
};

template< typename Item, std::size_t DEPTH > struct Trace< Item, std::integral_constant< std::size_t, DEPTH > > : std::array< Item, DEPTH > // TODO: use static_vector
{
	using Base = std::array< Item, DEPTH >;

	constexpr Trace( std::integral_constant< std::size_t, DEPTH > )
	{
	}

	constexpr Item& push_back( const Item& item )
	{
		assert( sz < DEPTH );
		return operator[]( sz++ ) = item;
	}

	template< typename... Ts > requires std::is_constructible_v< Item, Ts... > constexpr  Item& emplace_back( Ts&&... args )
	{
		assert( sz < DEPTH );
		return operator[]( sz++ ) = Item{ std::forward<Ts>(args)... };
	}

	constexpr void pop_back()
	{
		assert( sz > 0 );
		return operator[]( --sz ) = Item{};
	}

	constexpr Item& back()
	{
		return Base::operator[]( sz-1 );
	}

	constexpr const Item& back() const
	{
		return Base::operator[]( sz-1 );
	}

	constexpr bool empty() const { return sz == 0; }
	constexpr bool full() const { return sz == DEPTH; }

	std::size_t sz;
};

template< typename Item > struct Trace< Item, void > : std::vector<Item>
{
	using Base = std::vector<Item>;

	constexpr Trace()
	{
		Base::reserve(16);
	}

	constexpr bool full() const { return false; }
};

template< typename Tree, typename Depth > struct Iterator
{
	using Step = std::conditional_t< std::is_const_v<Tree>, typename Tree::const_iterator, typename Tree::iterator >;

	Tree* node;
	Trace< Step, Depth > trace;

	constexpr Iterator( Tree& tree ) requires std::is_void_v<Depth> : node( &tree )
	{
	}

	template< std::same_as<Depth> T > requires (not std::is_void_v<T>)
	constexpr Iterator( Tree& tree, T depth ) : node( &tree ), trace(depth)
	{
	}

	constexpr bool operator==(Sentinel) const
	{
		return !node;
	}

	bool operator==( const Iterator& other ) const
	{
		return node == other.node;
	}

	constexpr Iterator& operator++()
	{
		if( !node->empty() && !trace.full() )
		{
			const auto it = node->begin();
			trace.push_back(it);

			node = &*it;
		}
		else while( node = node->parent() )
		{
			if( auto& it = trace.back(); ++it != node->end() )
			{
				node = &*it;
				break;
			}

			trace.pop_back();
		}

		return *this;
	}

	constexpr auto operator*() const
	{
		return std::make_pair( trace | std::views::transform( [] (auto it) { return key( *it ); } ), std::ref( *node ) );
	}

	auto path() const
	{
		return trace | std::views::transform( [] (auto it) { return key( *it ); } );
	}

	using value_type = decltype( *std::declval<Iterator>() );
	using reference = value_type;
	using iterator_category = std::input_iterator_tag;
};

template< typename Tree > Iterator( Tree& ) -> Iterator< Tree, void >;

template< typename Tree, typename Depth > struct View : std::ranges::view_interface< View< Tree, Depth > >
{
	constexpr View( Tree& tree ) : tree( &tree )
	{
	}

	constexpr View( const View& ) = default;
	constexpr View& operator=( const View& ) = default;

	constexpr View( View&& ) = default;
	constexpr View& operator=( View&& ) = default;

	constexpr auto begin() const
	{
		return Iterator< Tree, Depth >{ *tree };
	}

	constexpr auto end() const
	{
		return Sentinel{};
	}

	constexpr bool empty() const { return false; }

	Tree* tree;
};

template< typename Tree > struct View< Tree, std::size_t > : View< Tree, void >
{
	using Base = View< Tree, void >;

	constexpr View( Tree& tree, std::size_t depth ) : Base(tree), depth(depth)
	{
	}

	constexpr auto begin() const
	{
		return Iterator< Tree, std::size_t >{ Base::tree, depth };
	}

	const std::size_t depth;
};

} // namespace traverse

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
template< path< Key<Value> > Path > auto KeyTree< Value, Comparison, Allocator >::trace( Path&& path )
{
	using namespace trace;
	return View{ *this, std::forward<Path>(path) };
}

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
template< path< Key<Value> > Path > auto KeyTree< Value, Comparison, Allocator >::trace( Path&& path ) const
{
	using namespace trace;
	return View{ *this, std::forward<Path>(path) };
}

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
auto KeyTree< Value, Comparison, Allocator >::traverse()
{
	using namespace traverse;
	return View< KeyTree, void >{ *this };
}

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
auto KeyTree< Value, Comparison, Allocator >::traverse() const
{
	using namespace traverse;
	return View< const KeyTree, void >{ *this };
}

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
auto KeyTree< Value, Comparison, Allocator >::traverse( std::size_t depth )
{
	using namespace traverse;
	return View< KeyTree, std::size_t >{ *this, depth };
}

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
auto KeyTree< Value, Comparison, Allocator >::traverse( std::size_t depth ) const
{
	using namespace traverse;
	return View< const KeyTree, std::size_t >{ *this, depth };
}

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
template< std::size_t DEPTH > auto KeyTree< Value, Comparison, Allocator >::traverse( std::integral_constant< std::size_t, DEPTH > depth )
{
	using namespace traverse;
	return View< KeyTree, decltype(depth) >{ *this };
}

template< typename Value, comparison< Key<Value> > Comparison, typename Allocator >
template< std::size_t DEPTH > auto KeyTree< Value, Comparison, Allocator >::traverse( std::integral_constant< std::size_t, DEPTH > depth ) const
{
	using namespace traverse;
	return View< const KeyTree, decltype(depth) >{ *this };
}

} // namespace detail::ktree

using detail::ktree::KeyTree;

} // namespace gain

#endif // !__GAIN_KEYTREE_HEADER__
