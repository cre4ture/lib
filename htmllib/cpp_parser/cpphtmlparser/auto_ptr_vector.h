/*
Pose Estimation with MEFU, Drei Augen Projekt, HS-Augsburg 2010
Copyright (C) 2011 Ulrich Hornung

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/
#ifndef MEFU_AUTO_PTR_VECTOR_H
#define MEFU_AUTO_PTR_VECTOR_H

/**
	Teil der Bachelorarbeit
	Analyse und Optimierung eines Multi-Kamera-Systems zur Bestimmung der Pose von Objekten
	HS-Augsburg, 2011
	@author: Ulrich Hornung (MEFU)
*/

namespace mefu {

	/** This is a vector of pointer, providing an auto delete functionality
	 * for all elements in list when destructor is called.
	 */
	template <class _Tp>
	class auto_ptr_vector: public std::vector<_Tp*>
	{
    private:
        // this complex stuff is needed only for the gcc!!!
        typedef std::vector<_Tp*> Base;
        typedef typename Base::iterator iterator;
        // complex stuff end...

		void deleteAll() {
			// delete all elements
			for (iterator i = Base::begin(); i != Base::end(); i++) {
				delete (*i);
			}
		}

	public:

		void clear() {
			deleteAll();
			std::vector<_Tp*>::clear();
		}

		auto_ptr_vector(void)
		{
		}

		~auto_ptr_vector(void)
		{
			deleteAll();
		}
	};

}

#endif // MEFU_AUTO_PTR_VECTOR_H
