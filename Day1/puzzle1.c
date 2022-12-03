/* 
 * Given input of strings of numbers separated by newlines, and sets separated by 
 * blank lines, find the set with the greatest sum
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_BUF 1024

//global counter for highest sum
long big_sum;
int greatest_set;

//takes the sum of each set and compares it to the global largest sum
void compare_sums(int sum, int *set_index)
{
	if (sum > big_sum){
		big_sum = sum;
		greatest_set = *set_index;
	}
	(*set_index)++;
}

int main()
{
	int set_index = 0;
	int sum = 0;
	char input_file[] = "sets.in";
	char input_line[MAX_BUF];
	//open the input file
	FILE *fp = fopen(input_file, "r");
	
	if (fp == NULL){
		exit(0);
	}

	while (fgets(input_line, MAX_BUF, fp)){
		//sum each line
		if (atoi(input_line)){
			sum += atoi(input_line);	
		}else{
			//compare sum to current greatest
			compare_sums(sum, &set_index);
			sum = 0; 
		}
	}

	//last set may not be followed by a blank line
	compare_sums(sum, &set_index);

	fclose(fp);
	//print greatest
	printf("Greatest set was %d of sum %d\n", greatest_set, big_sum);

	return 0;
}
