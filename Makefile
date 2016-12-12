all:
	make -C lib
	make -C src
	cp src/sigs_diff sigs_diff

clean:

	make -C src clean
	make -C lib clean
	rm sigs_diff
