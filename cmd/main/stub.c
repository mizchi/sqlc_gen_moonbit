#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// Global buffer for stdin data
static uint8_t* g_stdin_buffer = NULL;
static int64_t g_stdin_size = 0;

// Read all data from stdin and store in global buffer
// Returns the number of bytes read, or -1 on error
int64_t moonbit_read_stdin_size(void) {
    if (g_stdin_buffer != NULL) {
        return g_stdin_size;  // Already read
    }

    size_t capacity = 4096;
    size_t size = 0;
    uint8_t* buffer = (uint8_t*)malloc(capacity);

    if (buffer == NULL) {
        return -1;
    }

    while (1) {
        size_t bytes_read = fread(buffer + size, 1, capacity - size, stdin);
        size += bytes_read;

        if (bytes_read == 0) {
            break;  // EOF or error
        }

        if (size == capacity) {
            capacity *= 2;
            uint8_t* new_buffer = (uint8_t*)realloc(buffer, capacity);
            if (new_buffer == NULL) {
                free(buffer);
                return -1;
            }
            buffer = new_buffer;
        }
    }

    g_stdin_buffer = buffer;
    g_stdin_size = (int64_t)size;
    return g_stdin_size;
}

// Copy stdin data to provided buffer
void moonbit_copy_stdin_data(uint8_t* dest) {
    if (g_stdin_buffer != NULL && g_stdin_size > 0) {
        memcpy(dest, g_stdin_buffer, (size_t)g_stdin_size);
    }
}

// Write data to stdout
int moonbit_write_all_stdout(const uint8_t* data, int64_t len) {
    size_t written = fwrite(data, 1, (size_t)len, stdout);
    fflush(stdout);
    return written == (size_t)len ? 0 : -1;
}

// Write data to stderr
int moonbit_write_stderr(const uint8_t* data, int64_t len) {
    size_t written = fwrite(data, 1, (size_t)len, stderr);
    fflush(stderr);
    return written == (size_t)len ? 0 : -1;
}
