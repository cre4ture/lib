#ifndef MEFU_AUTO_PTR_MAP_H
#define MEFU_AUTO_PTR_MAP_H

/**
	@author: Ulrich Hornung (MEFU)
*/

#include <map>

namespace mefu {
	
	/** This is a map of pointer, providing an auto delete functionality
	 * for all elements in map when destructor is called.
	 */
	template <class _Tkey, class Tvalue>
	class auto_ptr_map: public std::map<_Tkey, Tvalue*>
	{
    private:
        // this complex stuff is needed only for the gcc!!!
        typedef std::map<_Tkey, Tvalue*> Base;
        // complex stuff end...

		void deleteAll() {
			// delete all elements
			for (iterator i = Base::begin(); i != Base::end(); i++) {
				delete (i->second);
			}
		}

	public:

        typedef typename Base::iterator iterator;

		void clear() {
			deleteAll();
			Base::clear();
		}

		bool elementExists(_Tkey &key)
		{
			iterator i = Base::find(key);
			return (i != Base::end());
		}

		auto_ptr_map(void)
		{
		}

		~auto_ptr_map(void)
		{
			deleteAll();
		}
	};

} // namespace mefu end

#endif