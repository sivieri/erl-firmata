#include <librs232/rs232.h>
#include <erl_nif.h>
#include <errno.h>
#include <unistd.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

typedef struct {
  ErlNifEnv *env;
  int speed;
  struct rs232_port_t *serial;
} state_t;

extern int errno;

static state_t state;

static ERL_NIF_TERM open_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    int error, local_speed;
    char device[RS232_STRLEN_DEVICE];

    if (argc != 2 || !enif_is_list(env, argv[0]) || !enif_is_number(env, argv[1])) {
        return enif_make_badarg(env);
    }
    if (!enif_get_string(env, argv[0], device, RS232_STRLEN_DEVICE, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }
    if (!enif_get_int(env, argv[1], &local_speed)) {
    	return enif_make_badarg(env);
    }
    switch (local_speed) {
    	case 300:
    		state.speed = RS232_BAUD_300;
    		break;
    	case 2400:
			state.speed = RS232_BAUD_2400;
			break;
    	case 4800:
			state.speed = RS232_BAUD_4800;
			break;
    	case 9600:
			state.speed = RS232_BAUD_9600;
			break;
    	case 19200:
			state.speed = RS232_BAUD_19200;
			break;
    	case 38400:
			state.speed = RS232_BAUD_38400;
			break;
    	case 57600:
			state.speed = RS232_BAUD_57600;
			break;
    	case 115200:
			state.speed = RS232_BAUD_115200;
			break;
    	case 460800:
			state.speed = RS232_BAUD_460800;
			break;
    	default:
    		return enif_make_badarg(env);
    }
    if (state.serial != NULL && rs232_port_open(state.serial)) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "serial port already opened", ERL_NIF_LATIN1));
    }
    state.env = env;
    state.serial = rs232_init();
    rs232_set_device(state.serial, device);
    error = rs232_open(state.serial);
    if (error > RS232_ERR_NOERROR) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, error));
    }
    rs232_set_baud(state.serial, state.speed);
    rs232_set_data(state.serial, RS232_DATA_8);
    rs232_set_parity(state.serial, RS232_PARITY_NONE);
    rs232_set_stop(state.serial, RS232_STOP_1);
    rs232_set_flow(state.serial, RS232_FLOW_OFF);
    
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM close_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (state.serial != NULL && rs232_port_open(state.serial)) {
        rs232_close(state.serial);
    }
    
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM flush_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (state.serial != NULL && rs232_port_open(state.serial)) {
        rs232_flush(state.serial);
    }

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM iflush_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (state.serial != NULL && rs232_port_open(state.serial)) {
        rs232_iflush(state.serial);
    }

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM oflush_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (state.serial != NULL && rs232_port_open(state.serial)) {
        rs232_oflush(state.serial);
    }

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM read_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    int length, error, timeout;
    unsigned int rlength;
    char *buffer;
    ERL_NIF_TERM term;
    
    if (argc != 2 || !enif_is_number(env, argv[0]) || !enif_is_number(env, argv[1])) {
        return enif_make_badarg(env);
    }
    if (!enif_get_int(env, argv[0], &length)) {
        return enif_make_badarg(env);
    }
    if (!enif_get_int(env, argv[1], &timeout)) {
    	return enif_make_badarg(env);
    }
    buffer = enif_alloc(length);
    memset(buffer, 0, length);
    if (state.serial == NULL || !rs232_port_open(state.serial)) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "serial port is closed", ERL_NIF_LATIN1));
    }
    error = rs232_read_timeout(state.serial, (unsigned char *) buffer, length, &rlength, timeout);
    term = enif_make_string_len(env, buffer, rlength, ERL_NIF_LATIN1);
    enif_free(buffer);
    
    return enif_make_tuple2(env, enif_make_int(env, error), term);
}

static ERL_NIF_TERM write_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary buffer;
    int error;
    unsigned int wlength;
    
    if (argc != 1 || !enif_is_binary(env, argv[0])) {
        return enif_make_badarg(env);
    }
    if (!enif_inspect_binary(env, argv[0], &buffer)) {
        return enif_make_badarg(env);
    }
    if (state.serial == NULL || !rs232_port_open(state.serial)) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "serial port is closed", ERL_NIF_LATIN1));
    }
    error = rs232_write(state.serial, buffer.data, buffer.size, &wlength);
    if (error > RS232_ERR_NOERROR) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, error));
    }
    else {
        return enif_make_atom(env, "ok");
    }
}

static ErlNifFunc rs232_NIFs[] = {
    {"open", 2, &open_2},
    {"close", 0, &close_0},
    {"flush", 0, &flush_0},
    {"iflush", 0, &iflush_0},
    {"oflush", 0, &oflush_0},
    {"read", 2, &read_2},
    {"write", 1, &write_1}
};

ERL_NIF_INIT(rs232, rs232_NIFs, NULL, NULL, NULL, NULL);
