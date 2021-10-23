class Literals {
    public static void main(String[] args) {
	// various syntactic forms of literal
	// most examples drawn from the JLS, see here:
	// https://docs.oracle.com/javase/specs/jls/se7/html/jls-3.html#jls-3.10.4
	
	// bools
	boolean t = true;
	boolean f = false;

	// ints/longs
	int a = 42; // decimal
	int b = 0x2A; // hex
	int c = 052; // octal
	int d = 0b00101010; // binary
	// same as above, underscores should be ignored
	int a1 = 4_2;
	int b1 = 0x2_A;
	int c1 = 05_2;
	int d1 = 0b00_10_10_10;
	// same as above, l/L suffix denotes `long` type
	long a2 = 42l;
	long b2 = 0x2al;
	long c2 = 052l;
	long d2 = 0b00101010l;
	int binary64bit = 0b1010101010101010101010101010101010101010101010101010101010101010;

	// floats/doubles
	float a3 = 1e1f;
	float b3 = 2.f;
	float c3 = .3f;
	float d3 = 0f;
	float e3 = 3.14f;
	float f3 = 6.022137e+23f;
	double g3 = 1e1;
	double h3 = 2.;
	double i3 = .3;
	double j3 = 0.0;
	double k3 = 3.14;
	double l3 = 1e-9d;
	double m3 = 1e137;
	
	// chars (java treats as UTF-16 value)
	char a4 = 'a'; // regular old ascii (= 97)
	char b4 = 'α'; // unicode \u03B1
	char c4 = 100; // ascii value of 'd'
	// char escape sequences
	char d4 = '\t';
	char e4 = '\'';
	char f4 = '\\';
	char g4 = '\n';
	char h4 = '\r';
	char i4 = '\"';

	// strings
	String a5 = "foo";
	String b5 = "";
	String c5 = "\\\n";
    }

}
