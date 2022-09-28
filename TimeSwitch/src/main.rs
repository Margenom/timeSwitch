fn main() {
	let argv = ["-t", "-test=4", "data3"];
	for arg in argv { 
		let b = arg.as_bytes();
		if b[0] == '-' { println!("{}",arg);}
	}
}
