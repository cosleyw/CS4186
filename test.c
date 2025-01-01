int fac(int k){
	if(k == 0)
		return 1;

	return fac(k - 1) * k;
}

int main(int a){
	return fac(5);
}

