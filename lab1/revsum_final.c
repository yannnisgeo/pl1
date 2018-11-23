#include <stdio.h>
#include <stdlib.h>

int A[100005];

int B[100005];

int Length, middle;

int element(int initlcarry){
	int ileft = 1;
	int iright = Length;
	int lcarry = initlcarry;
	int rcarry = 0;
	int impossible = 0;
	int crleft = 1;
	int crright = Length;

	while (ileft<iright){
		int l = A[ileft];
		int r = A[iright];
		if (!lcarry && !rcarry) {
			if (l==r) {
				lcarry = 0;
				rcarry = 0;
				B[crleft] = l;
				B[crright] = 0;
			}
			else if (l==(r+1)%10){
				lcarry = 1;
				rcarry = 0;
				B[crleft] = r;
				B[crright] = 0;
			}
			else{
				impossible = 1;
				break;
			}
		}
		else if (!lcarry && rcarry) {
			if (l==r) {
				if (l>0) {
					lcarry = 1;
					rcarry = 0;
					B[crleft] = (l-1)%10;
					B[crright] = 0;
				}
				else {
					lcarry = 0;
					rcarry = 1;
					B[crleft] = 0;
					B[crright] = 9;
				}
			}
			else if (l==(r-1)%10){
				lcarry = 0;
				rcarry = 0;
				B[crleft] = l;
				B[crright] = 0;
			}
			else{
				impossible = 1;
				break;
			}
		}
		else if (lcarry && !rcarry) {
			if ((l==r) && (l<9)) {
				lcarry = 0;
				rcarry = 1;
				B[crleft] = 9;
				B[crright] = l+1;
			}
			else if ((l==(r+1)%10) && (l>0)){
				lcarry = 1;
				rcarry = 1;
				B[crleft] = 9;
				B[crright] = r;
			}
			else{
				impossible = 1;
				break;
			}
		}
		else if (lcarry && rcarry) {
			if (l==r) {
				lcarry = 1;
				rcarry = 1;
				B[crleft] = 9;
				B[crright] = l;
			}
			else if ((l==(r-1)%10) && (l<9)){
				lcarry = 0;
				rcarry = 1;
				B[crleft] = 9;
				B[crright] = l+1;
			}
			else{
				impossible = 1;
				break;
			}
		}

		iright--;
		crright--;
		ileft++;
		crleft++;
	}

	if (impossible) return 0;
	if (iright==ileft) {
		if (!lcarry && !rcarry){
			if (middle%2==0) B[iright] = middle/2;
			else impossible = 1;
		}
		else if (!lcarry && rcarry){
			if (middle%2==1) B[iright] = middle/2;
			else impossible = 1;
		}
		else if (lcarry && !rcarry){
			if (middle%2==0) B[iright] = middle/2+5;
			else impossible = 1;
		}
		else if (lcarry && rcarry){
			if (middle%2==1) B[iright] = middle/2+5;
			else impossible = 1;
		}
	}
	else{
		if (lcarry!=rcarry) impossible = 1;
	}
	if (impossible) return 0;

	return 1;
}



int main(int argc, char* argv[]){
	FILE *f = fopen(argv[1], "r");
	//FILE *f = fopen("mufa.txt", "r");
	char c;
	int i = 0;
	while (1){
        	if (fscanf(f, "%c", &c)==0) break;
		if ((c>='0') && (c<='9')){
			i++;
			A[i] = c-'0';
			B[i]=0;
		}
		else break;
	}
	Length = i;
	//for(i=1; i<=Length;i++) printf("%d ", A[i]);
	//printf("%d\n", Length);
	if (Length%2==0) middle = -1;
	else middle = A[Length/2+1];

	int ans = element(0);
	if (ans) {
		for(i=1;i<=Length;i++) printf("%d",B[i]);
	}
	else{
		if (A[1]==1){
			for(i=1; i<=Length; i++) A[i-1]=A[i];
			Length--;
			if (Length%2==0) middle = -1;
			else middle = A[Length/2+1];
			int ans = element(1);
			if (ans) {
				for(i=1;i<=Length;i++) printf("%d",B[i]);
			}
			else{
				printf("0");
			}
		}
		else printf("0");
	}
}
