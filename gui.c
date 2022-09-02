#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "s7.h"

s7_pointer gui_input_mesg(s7_scheme *sc, s7_pointer args) {
	s7_pointer Ret = s7_f(sc);
	if (!(s7_list_length(sc, args) >0 && s7_is_string(s7_car(args))))
		return Ret;

	GtkWidget *Box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	GtkWidget *Msg = gtk_entry_new();
	gtk_entry_set_width_chars(GTK_ENTRY(Msg), 96);
	gtk_box_pack_start(GTK_BOX(Box), Msg, TRUE, TRUE, 0);

	GtkWidget *Step = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(Step), s7_string(s7_car(args)));
	gtk_window_set_resizable(GTK_WINDOW(Step), FALSE);

	void step_quit(GtkWidget *wid, gpointer *udata) {
		gtk_main_quit();
	gtk_widget_destroy(Step);
	}
	g_signal_connect(Step, "destroy", G_CALLBACK (step_quit), NULL);
	void step_gone(GtkWidget *wid, gpointer *udata) {
		Ret =  s7_make_string(sc,gtk_entry_get_text(GTK_ENTRY(Msg)));
		gtk_main_quit();
	gtk_widget_destroy(Step);
	}
	g_signal_connect(Msg, "activate", G_CALLBACK (step_gone), NULL);
	gtk_container_add(GTK_CONTAINER(Step), Box);
	gtk_widget_show_all(Step);

	gtk_main();	
	return Ret;
}

void gui(s7_scheme *sc) {
	gtk_init(NULL, NULL);
	s7_define_function(sc, "gui-input-mesg", gui_input_mesg, 1, 0, 0, "Toplevel dialog for input line text messg (gui-input-mesg title): mesg");
}
