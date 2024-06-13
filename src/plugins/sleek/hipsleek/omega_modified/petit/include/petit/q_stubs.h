/* $Id: q_stubs.h,v 1.1 2010-12-10 07:42:13 locle Exp $ */
#ifndef Already_Included_Q_Stubs
#define Already_Included_Q_Stubs

namespace omega {

extern int quantify_record_data ();
extern int quantify_start_recording_data ();
extern int quantify_stop_recording_data ();
extern int quantify_record_system_calls ();
extern int quantify_start_recording_system_calls ();
extern int quantify_stop_recording_system_calls ();
extern int quantify_record_register_window_traps ();
extern int quantify_start_recording_register_window_traps ();
extern int quantify_stop_recording_register_window_traps ();
extern int quantify_clear_data ();
extern int quantify_save_data ();
extern int quantify_save_data_to_file ();
extern int quantify_add_annotation ();
extern int quantify_help();
extern int quantify_print_recording_state();

}

#endif
