#ifndef VECTOR_H_
#define VECTOR_H_

/** @file vector.h
 *
 * An implementation of dynamic array.
 *
 * Example:
 * @code
 * size_t i;
 * Vector(int, vec);
 * VectorPushBack(vec, 1);
 * VectorPushBack(vec, 2);
 * VectorPushBack(vec, 3);
 * for ( i = 0; i < VectorSize(vec); i++ )
 *   printf("%d\n", VectorPtr(vec)[i]);
 * VectorFree(vec);
 * @endcode
 */
/* #[ License : */
/*
 *   Copyright (C) 1984-2017 J.A.M. Vermaseren
 *   When using this file you are requested to refer to the publication
 *   J.A.M.Vermaseren "New features of FORM" math-ph/0010025
 *   This is considered a matter of courtesy as the development was paid
 *   for by FOM the Dutch physics granting agency and we would like to
 *   be able to track its scientific use to convince FOM of its value
 *   for the community.
 *
 *   This file is part of FORM.
 *
 *   FORM is free software: you can redistribute it and/or modify it under the
 *   terms of the GNU General Public License as published by the Free Software
 *   Foundation, either version 3 of the License, or (at your option) any later
 *   version.
 *
 *   FORM is distributed in the hope that it will be useful, but WITHOUT ANY
 *   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 *   details.
 *
 *   You should have received a copy of the GNU General Public License along
 *   with FORM.  If not, see <http://www.gnu.org/licenses/>.
 */
/* #] License : */ 
/*
  	#[ Includes :
*/

#include <stddef.h>
#include <string.h>
#include "declare.h"

/*
  	#] Includes : 
  	#[ Vector :
 		#[ VectorStruct :
*/

/**
 * A struct for vector objects.
 *
 * @param  T  the type of elements.
 */
#define VectorStruct(T) \
	struct { \
		T *ptr; \
		size_t size; \
		size_t capacity; \
	}

/*
 		#] VectorStruct : 
 		#[ Vector :
*/

/**
 * Defines and initialises a vector X of the type T.
 * The user must call VectorFree(X) after the use of X.
 *
 * @param  T  the type of elements.
 * @param  X  the name of vector object
 */
#define Vector(T, X) \
	VectorStruct(T) X = { NULL, 0, 0 }

/*
 		#] Vector : 
 		#[ DeclareVector :
*/

/**
 * Declares a vector X of the type T.
 * The user must call VectorInit(X) before the use of X.
 *
 * @param  T  the type of elements.
 * @param  X  the name of vector object
 */
#define DeclareVector(T, X) \
	VectorStruct(T) X

/*
 		#] DeclareVector : 
 		#[ VectorInit :
*/

/**
 * Initialises a vector X of the type T.
 * The user must call VectorFree(X) after the use of X.
 *
 * @param  X  the vector object.
 */
#define VectorInit(X) \
	do { \
		(X).ptr = NULL; \
		(X).size = 0; \
		(X).capacity = 0; \
	} while (0)

/*
 		#] VectorInit : 
 		#[ VectorFree :
*/

/**
 * Frees the buffer allocated by the vector X.
 *
 * @param  X  the vector object.
 */
#define VectorFree(X) \
	do { \
		M_free((X).ptr, "VectorFree:" #X); \
		(X).ptr = NULL; \
		(X).size = 0; \
		(X).capacity = 0; \
	} while (0)

/*
 		#] VectorFree : 
 		#[ VectorPtr :
*/

/**
 * Returns the pointer to the buffer for the vector X.
 * NULL when VectorCapacity(X) == 0.
 *
 * @param  X  the vector object.
 * @return    the pointer to the allocated buffer for the vector.
 */
#define VectorPtr(X) \
	((X).ptr)

/*
 		#] VectorPtr : 
 		#[ VectorFront :
*/

/**
 * Returns the first element of the vector X.
 * Undefined when VectorSize(X) == 0.
 *
 * @param  X  the vector object.
 * @return    the first element of the vector.
 */
#define VectorFront(X) \
	((X).ptr[0])

/*
 		#] VectorFront : 
 		#[ VectorBack :
*/

/**
 * Returns the last element of the vector X.
 * Undefined when VectorSize(X) == 0.
 *
 * @param  X  the vector object.
 * @return    the last element of the vector.
 */
#define VectorBack(X) \
	((X).ptr[(X).size - 1])

/*
 		#] VectorBack : 
 		#[ VectorSize :
*/

/**
 * Returns the size of the vector X.
 *
 * @param  X  the vector object.
 * @return    the size of the vetor.
 */
#define VectorSize(X) \
	((X).size)

/*
 		#] VectorSize : 
 		#[ VectorCapacity :
*/

/**
 * Returns the capacity (the number of the already allocated elements) of the vector X.
 *
 * @param  X  the vector object.
 * @return    the capacity of the vetor.
 */
#define VectorCapacity(X) \
	((X).capacity)

/*
 		#] VectorCapacity : 
 		#[ VectorEmpty :
*/

/**
 * Returns true the size of the vector X is zero.
 *
 * @param  X  the vector object.
 * @return    true if the vector has no elements, false otherwise.
 */
#define VectorEmpty(X) \
	((X).size == 0)

/*
 		#] VectorEmpty : 
 		#[ VectorClear :
*/

/**
 * Sets the size of the vector X to zero.
 *
 * @param  X  the vector object.
 */
#define VectorClear(X) \
	do { (X).size = 0; } while (0)

/*
 		#] VectorClear : 
 		#[ VectorReserve :
*/

/**
 * Requires that the capacity of the vector X is equal to or lager than newcapacity.
 *
 * @param  X            the vector object.
 * @param  newcapacity  the capacity to be reserved.
 */
#define VectorReserve(X, newcapacity) \
	do { \
		size_t v_tmp_newcapacity_ = (newcapacity); \
		if ( (X).capacity < v_tmp_newcapacity_ ) { \
			void *v_tmp_newptr_; \
			v_tmp_newcapacity_ = (v_tmp_newcapacity_ * 3) / 2; \
			if ( v_tmp_newcapacity_ < 4 ) v_tmp_newcapacity_ = 4; \
			v_tmp_newptr_ = Malloc1(sizeof((X).ptr[0]) * v_tmp_newcapacity_, "VectorReserve:" #X); \
			if ( (X).ptr != NULL ) { \
				memcpy(v_tmp_newptr_, (X).ptr, (X).size * sizeof((X).ptr[0])); \
				M_free((X).ptr, "VectorReserve:" #X); \
			} \
			(X).ptr = v_tmp_newptr_; \
			(X).capacity = v_tmp_newcapacity_; \
		} \
	} while (0)

/*
 		#] VectorReserve : 
 		#[ VectorPushBack :
*/

/**
 * Adds an element x at the end of the vector X.
 *
 * @param  X  the vector object.
 * @param  x  the element to be added.
 */
#define VectorPushBack(X, x) \
	do { \
		VectorReserve((X), (X).size + 1); \
		(X).ptr[(X).size++] = (x); \
	} while (0)

/*
 		#] VectorPushBack : 
 		#[ VectorPushBacks :
*/

/**
 * Adds an n elements of src at the end of the vector X.
 *
 * @param  X    the vector object.
 * @param  src  the starting address of the buffer storing elements to be added.
 * @param  n    the number of elements to be added.
 */
#define VectorPushBacks(X, src, n) \
	do { \
		size_t v_tmp_n_ = (n); \
		VectorReserve((X), (X).size + v_tmp_n_); \
		memcpy((X).ptr + (X).size, (src), v_tmp_n_ * sizeof((X).ptr[0])); \
		(X).size += v_tmp_n_; \
	} while (0)

/*
 		#] VectorPushBacks : 
 		#[ VectorPopBack :
*/

/**
 * Removes the last element of the vector X.
 * VectorSize(X) must be > 0.
 *
 * @param  X  the vector object.
 */
#define VectorPopBack(X) \
	do { (X).size --; } while (0)

/*
 		#] VectorPopBack : 
 		#[ VectorInsert :
*/

/**
 * Inserts an element x at the specified index of the vector X.
 * The index must be 0 <= index < VectorSize(X).
 *
 * @param  X      the vector object.
 * @param  index  the position at which the element will be inserted.
 * @param  x      the element to be inserted.
 */
#define VectorInsert(X, index, x) \
	do { \
		size_t v_tmp_index_ = (index); \
		VectorReserve((X), (X).size + 1); \
		memmove((X).ptr + v_tmp_index_ + 1, (X).ptr + v_tmp_index_, ((X).size - v_tmp_index_) * sizeof((X).ptr[0])); \
		(X).ptr[v_tmp_index_] = (x); \
		(X).size++; \
	} while (0)

/*
 		#] VectorInsert : 
 		#[ VectorInserts :
*/

/**
 * Inserts an n elements of src at the specified index of the vector X.
 * The index must be 0 <= index < VectorSize(X).
 *
 * @param  X      the vector object.
 * @param  index  the position at which the elements will be inserted.
 * @param  src    the starting address of the buffer storing elements to be inserted.
 * @param  n      the number of elements to be inserted.
 */
#define VectorInserts(X, index, src, n) \
	do { \
		size_t v_tmp_index_ = (index), v_tmp_n_ = (n); \
		VectorReserve((X), (X).size + v_tmp_n_); \
		memmove((X).ptr + v_tmp_index_ + v_tmp_n_, (X).ptr + v_tmp_index_, ((X).size - v_tmp_index_) * sizeof((X).ptr[0])); \
		memcpy((X).ptr + v_tmp_index_, (src), v_tmp_n_ * sizeof((X).ptr[0])); \
		(X).size += v_tmp_n_; \
	} while (0)

/*
 		#] VectorInserts : 
 		#[ VectorErase :
*/

/**
 * Removes an element at the specified index of the vector X.
 * The index must be 0 <= index < VectorSize(X).
 *
 * @param  X      the vector object.
 * @param  index  the position of the element to be removed.
 */
#define VectorErase(X, index) \
	do { \
		size_t v_tmp_index_ = (index); \
		memmove((X).ptr + v_tmp_index_, (X).ptr + v_tmp_index_ + 1, ((X).size - v_tmp_index_ - 1) * sizeof((X).ptr[0])); \
		(X).size--; \
	} while (0)

/*
 		#] VectorErase : 
 		#[ VectorErases :
*/

/**
 * Removes an n elements at the specified index of the vector X.
 * The index must be 0 <= index < VectorSize(X) - n + 1.
 *
 * @param  X      the vector object.
 * @param  index  the starting position of the elements to be removed.
 * @param  n      the number of elements to be removed.
 */
#define VectorErases(X, index, n) \
	do { \
		size_t v_tmp_index_ = (index), v_tmp_n_ = (n); \
		memmove((X).ptr + v_tmp_index_, (X).ptr + v_tmp_index_ + v_tmp_n_, ((X).size - v_tmp_index_ - 1) * sizeof((X).ptr[0])); \
		(X).size -= v_tmp_n_; \
	} while (0)

/*
 		#] VectorErases : 
  	#] Vector : 
*/
#endif  /* VECTOR_H_ */
