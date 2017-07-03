/*
 *********************************************
 *  314 Principles of Programming Languages  *
 *  Spring 2017                              *
 *  Author: Ulrich Kremer                    *
 *  Pranav Katkamwar (pranavk)               *
 *********************************************
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "InstrUtils.h"
#include "Utils.h"

void optimizer(Instruction* ptr, int field);
void optimize(Instruction* curr);

int main()
{
	Instruction *head;

	head = ReadInstructionList(stdin);
	if (!head) {
		WARNING("No instructions\n");
		exit(EXIT_FAILURE);
	}
	
	Instruction* curr = head;
	head-> prev = NULL;
	
	// Traverse to the bottom of the instruction list
	while(curr -> next != NULL){
		// Find when 1024 is stored into register 0
		if (curr->opcode == LOADI){
			if (curr->field1 == 1024){
				curr->critical = 'c';
			}
		}
		curr = curr -> next;
	}

	// Search for all occurences of OUTPUTAI
	while(curr -> prev != NULL){
		
		if (curr->opcode == OUTPUTAI){	
			curr->critical = 'c';
			if(curr != head){
				optimize(curr);
			}
		}
		curr = curr -> prev;
	}

	PrintInstructionList(stdout, head);	
	
	// Free all nodes
	curr = head;
	Instruction *temp;
	while (curr->next != NULL){
		temp = curr->next;
		free (curr);
		curr = temp;
	}
	
	return EXIT_SUCCESS;
}

// Recurse on an instruction
void optimize(Instruction* curr){
	
	Instruction* ptr;
	ptr = curr -> prev;
	
	while(ptr -> prev != NULL){
		if (curr->opcode == OUTPUTAI){
			if (curr->field2 == ptr->field3){
				if (ptr->opcode != LOADI){
					ptr->critical = 'c';
					optimize(ptr);
					return;
				}
			}
		}
		else if (curr->opcode == STOREAI){
			if ((curr->field1 == ptr->field1) || (curr->field1 == ptr->field2) || (curr->field1 == ptr->field3)){
				ptr->critical = 'c';
				optimize(ptr);
				return;
			}
		}
		else if (curr->opcode == LOADAI){
			if ((ptr->opcode == STOREAI) &&(curr->field2 == ptr->field3)){
				ptr->critical = 'c';
				optimize(ptr);
				return;
			}
		}
		else if (curr->opcode == LOADI){

		}
		else{
			if ((curr->field1 == ptr->field3) || (curr->field2 == ptr->field3)|| (curr->field1 == ptr->field2) || (curr->field2 == ptr->field2)){
				if (ptr->opcode != STOREAI){
					ptr -> critical = 'c';
					optimize(ptr);
				}
			}
		}
		ptr = ptr -> prev;
	}	
}
