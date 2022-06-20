#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

size_t OodleLZ_Decompress (uint8_t * srcBuf, size_t srcLen, uint8_t * dstBuf, size_t dstLen, int64_t u1, int64_t u2, int64_t u3, uint8_t * u4, size_t u5, void * u6, void * u7, void * u8, size_t u9, int64_t u10) {
	fputs ("[Error] Decompression support is unavailable due to the use of the dummy Linux Oodle library. Please see README.md for more information.\n", stderr);
	exit (-1);
	return 0;
}
