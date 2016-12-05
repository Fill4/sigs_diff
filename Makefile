all:
	bash -c "cd lib/; make"
	bash -c "cd src/; make; cp sigs_diff ../sigs_diff"

clean:
	bash -c "cd lib/; make clean"
	bash -c "cd src/; make clean"
