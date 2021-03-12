#include <iostream>


//Create a tombstone class
template <class T>
class Tombstone {
	public:
		T * tomb;
		Tombstone(){} //default constructor              
		Tombstone(T* t) {   //bootstrapping constructor       
			tomb = t;
		}
		~Tombstone() {              //Destructor
			free(tomb);        //deallocate the memory
			tomb = NULL;       //set object to NULL
		}
};


//Create a pointer class to reference tombstones
template <class T>
class Pointer {
	public:
		Tombstone<T> * pTomb; //pointer to tombstone

		Pointer<T>() { 			//constructor
			pTomb = new Tombstone<T>();	
		}

		Pointer<T>(const Pointer<T>& p){  //copy constructor
			pTomb = p.pTomb;
		}

		Pointer<T>(T* tomb) {			//bootstrapping constructor
			pTomb = new Tombstone<T>(tomb);
		}

		~Pointer<T>() {                     //Destructor
			pTomb->~Tombstone();
			pTomb = NULL;
		}
		T& operator*() const { //derefence
			if (pTomb->tomb != NULL) {
				return *(pTomb->tomb);
			}
			else {
				std::cout << "Dangling Pointer or Memory Leak! - Exiting";
				exit(-1);
			}
		}

		Pointer<T>& operator=(T* second) { //overload assignment
			pTomb->tomb = second;
	        	return *this;
	    }


        	T* operator->() const { //field dereferencing
			return pTomb->tomb;
		}

		void friend free(Pointer<T>& pTomb) { //delete pointed at object
			pTomb.pTomb->~Tombstone();	//calls destructor
			pTomb.pTomb->tomb = NULL;
		}
        
		Pointer<T>& operator=(const Pointer<T>& second) { //copy constructor
			Pointer<T>* copy = new Pointer<T>(second);
     	    *this = std::move(*copy);
	        return *this;
	    }

		

        // Begin overloading comparative operators
		bool operator==(const int x) const { 
			if(pTomb->tomb == NULL && x == 0){
				return true;

			}
			return false;
		}
		bool operator==(const Pointer<T>& p) const {  
			if(pTomb->tomb == p.pTomb->tomb){
				return true;
			}
			return false;
		}
		bool operator!=(const int x) const { 
			if(pTomb->tomb != NULL && x == 0){
				return true;
			}
			return false;
		}

		bool operator!=(const Pointer<T>& p) const { 
			if(bool isEqual = pTomb->tomb != p.pTomb->tomb){
				return true;
			}
			return false;
		}
};


template <class T>
bool operator==(const int x, const Pointer<T>& tomb) { return tomb == x; }
template <class T>
bool operator!=(const int x, const Pointer<T>& tomb) { return tomb != x; }
