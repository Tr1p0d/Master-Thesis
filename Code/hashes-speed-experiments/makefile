CC=g++
CFLAGS=-std=c++11 -Wall -O3
LINK=g++

TARGET=speedtest

default: $(TARGET)

$(TARGET): test.o MurmurHash3.o MurmurHash2.o City.o Spooky.o iphash.o
	$(CC) $(CFLAGS) -o $@ test.o iphash.o  MurmurHash3.o MurmurHash2.o Spooky.o City.o

iphash.o: iphash.cpp iphash.h
	$(CC) $(CFLAGS) -c iphash.cpp

test.o: test.cpp
	$(CC) $(CFLAGS) -c test.cpp

MurmurHash3.o: MurmurHash3.cpp MurmurHash3.h
	$(CC) $(CFLAGS) -c MurmurHash3.cpp

MurmurHash2.o: MurmurHash2.cpp MurmurHash2.h
	$(CC) $(CFLAGS) -c MurmurHash2.cpp

City.o: City.cpp City.h
	$(CC) $(CFLAGS) -c City.cpp

Spooky.o: Spooky.cpp Spooky.h
	$(CC) $(CFLAGS) -c Spooky.cpp

clean: 
	rm *.o
	rm speedtest
