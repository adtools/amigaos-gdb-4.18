/* i960 device support
   Copyright (C) 1997, 1998, 1999 Free Software Foundation, Inc.
   Contributed by Cygnus Solutions.

This file is part of GDB, the GNU debugger.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "sim-main.h"

#ifdef HAVE_DV_SOCKSER
#include "dv-sockser.h"
#endif

/* Handling the MSPR register is done by creating a device in the core
   mapping that winds up here.  */

device i960_devices;

int
device_io_read_buffer (device *me, void *source, int space,
		       address_word addr, unsigned nr_bytes,
		       SIM_CPU *cpu, sim_cia cia)
{
  SIM_DESC sd = CPU_STATE (cpu);

  if (STATE_ENVIRONMENT (sd) != OPERATING_ENVIRONMENT)
    return nr_bytes;

#ifdef HAVE_DV_SOCKSER
  if (addr == UART_INCHAR_ADDR)
    {
      int c = dv_sockser_read (sd);
      if (c == -1)
	return 0;
      *(char *) source = c;
      return 1;
    }
  if (addr == UART_STATUS_ADDR)
    {
      int status = dv_sockser_status (sd);
      unsigned char *p = source;
      p[0] = 0;
      p[1] = (((status & DV_SOCKSER_INPUT_EMPTY)
#ifdef UART_INPUT_READY0
	       ? UART_INPUT_READY : 0)
#else
	       ? 0 : UART_INPUT_READY)
#endif
	      + ((status & DV_SOCKSER_OUTPUT_EMPTY) ? UART_OUTPUT_READY : 0));
      return 2;
    }
#endif

  return nr_bytes;
}

int
device_io_write_buffer (device *me, const void *source, int space,
			address_word addr, unsigned nr_bytes,
			SIM_CPU *cpu, sim_cia cia)
{
  SIM_DESC sd = CPU_STATE (cpu);

#if WITH_SCACHE
  /* MSPR support is deprecated but is kept in for upward compatibility
     with existing overlay support.  */
  if (addr == MSPR_ADDR)
    {
      if ((*(const char *) source & MSPR_PURGE) != 0)
	scache_flush (sd);
      return nr_bytes;
    }
  if (addr == MCCR_ADDR)
    {
      if ((*(const char *) source & MCCR_CP) != 0)
	scache_flush (sd);
      return nr_bytes;
    }
#endif

  if (STATE_ENVIRONMENT (sd) != OPERATING_ENVIRONMENT)
    return nr_bytes;

#if HAVE_DV_SOCKSER
  if (addr == UART_OUTCHAR_ADDR)
    {
      int rc = dv_sockser_write (sd, *(char *) source);
      return rc == 1;
    }
#endif

  return nr_bytes;
}

void device_error () {}
