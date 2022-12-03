/* 
 * Given input of strings of numbers separated by newlines, and sets separated by 
 * blank lines, find the 3 sets with the greatest sum
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_BUF 1024

//global counter for highest sum
long big_sum;
long big_sum2;
long big_sum3;
int greatest_set;
long long total_sum;


//takes the sum of each set and compares it to the global largest sum
void compare_sums(int sum, int *set_index)
{
	if (sum > big_sum){
		big_sum3 = big_sum2;
		big_sum2 = big_sum;
		big_sum = sum;
		greatest_set = *set_index;
	} else if (sum > big_sum2){
		big_sum3 = big_sum2;
		big_sum2 = sum;
	} else if (sum > big_sum3){
		big_sum3 = sum;
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
	printf("Second greatest set was %d\n", big_sum2);
	printf("Third greatest set was %d\n", big_sum3);
	printf("Total greatest sets was %d\n", big_sum + big_sum2 + big_sum3);

	return 0;
}
