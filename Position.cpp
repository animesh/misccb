/*#include<iostream.h>
int main(){
	cout << "hi\n";
}*/
#include<iostream>

int main( int argc, char **argv )
{
	    std::cout << "Hello World" << std::endl;
	        return 0;
}

#include "Position.h"

// Constructors/Destructors
//  

//  
// Methods
//  


// Accessor methods
//  


// Public static attribute accessor methods
//  


// Public attribute accessor methods
//  


/**
 * Set the value of m_State
 * @param new_var the new value of m_State
 */
void Position::setState ( bool new_var ) {
  m_State = new_var;
}

/**
 * Get the value of m_State
 * @return the value of m_State
 */
bool Position::getState ( ) {
  return m_State;
}

// Protected static attribute accessor methods
//  


// Protected attribute accessor methods
//  


// Private static attribute accessor methods
//  


// Private attribute accessor methods
//  


// Other methods
//  


/**
 * @return char
 */
char Position::Emission ( ) {

}


