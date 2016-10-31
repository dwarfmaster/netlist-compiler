
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void* load_rom(const char* path) {
    FILE* fp = fopen(path, "rb");
    if(!fp) {
        printf("Could not open %s\n", path);
        exit(EXIT_FAILURE);
    }
    fseek(fp, 0, SEEK_END);
    long size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    void* data = malloc(size);
    fread(data, size, 1, fp);
    fclose(fp);

    return data;
}

void* load_ram(const char* num) {
    long size = atoi(num);
    void* m = malloc(size);
    return memset(m, 0, size);
}

