int fac(int k){
	if(k == 0)
		return 1;

	for(k = 0; k < 10; k++);

	return fac(k - 1) * k;
}

int main(int a){
	return fac(5);
}

