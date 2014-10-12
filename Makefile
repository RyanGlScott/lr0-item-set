GHC=/usr/bin/ghc
NAME=lr0-item-set
FILE=Main
RM=rm
TEST=input1.txt

all:
	$(GHC) $(FILE).hs -o $(NAME)

run: all
	./$(NAME)

clean:
	$(RM) $(FILE).hi
	$(RM) $(FILE).o
	$(RM) $(NAME)

test: all
	./$(NAME) < $(TEST) 
