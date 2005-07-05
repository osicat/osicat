/* Copyright (c) 2003 Nikodemus Siivola
 * 
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>
#include <pwd.h>
#include <errno.h>
#include <unistd.h>

extern int
osicat_mode (char * name, int follow_p)
{
    struct stat buf;
    int err;

    if (follow_p) {
	err = stat (name, &buf);
    }
    else {
	err = lstat (name, &buf);
    }

    if (! err)
	return buf.st_mode;
    else
	/* I assume that -1 is not a valid mode? */
	return -1;
}

extern char *
osicat_dirent_name (struct dirent * entry)
{
    return entry->d_name;
}

extern char *
osicat_getcwd (void)
{
    size_t size = 128;
    while (1)
	{
	    char *buffer = (char *) malloc (size);
	    if (!buffer) {
		return 0;
	    }
	    else if (getcwd (buffer, size) == buffer) {
		return buffer;
	    }
	    else {
		free (buffer);
		if (errno != ERANGE) 
		    return 0;
		size += 128;
	    }
	}
}

extern char *
osicat_pwent_name (struct passwd * pwent)
{
    return pwent->pw_name;
}

extern char *
osicat_pwent_passwd (struct passwd * pwent)
{
    return pwent->pw_passwd;
}

extern int
osicat_pwent_uid (struct passwd * pwent)
{
    return pwent->pw_uid;
}

extern int
osicat_pwent_gid (struct passwd * pwent)
{
    return pwent->pw_gid;
}

extern char *
osicat_pwent_gecos (struct passwd * pwent)
{
    return pwent->pw_gecos;
}

extern char *
osicat_pwent_home (struct passwd * pwent)
{
    return pwent->pw_dir;
}

extern char *
osicat_pwent_shell (struct passwd * pwent)
{
    return pwent->pw_shell;
}


#include <stdio.h>
#include <stdlib.h>

extern int
osicat_tmpfile (void)
{
    FILE *fp;

    fp = tmpfile ();
    if (fp == NULL) return -1;
    return fileno (fp);
}
