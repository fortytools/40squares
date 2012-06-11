RUN_OPTS = +RTS -N4 -RTS

all:
	ghc -rtsopts -Wall -threaded -feager-blackholing --make 40squares.hs

name:
	@echo "40squares"

run:
	@./40squares $(RUN_OPTS)

clean:
	rm 40squares

test:
	(echo "[2,4,40]"; echo "42"; cat) | ./40squares $(RUN_OPTS)

