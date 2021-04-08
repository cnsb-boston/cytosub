#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <time.h>

#include <R.h>
#include <Rinternals.h>
//#include <Rdefines.h>

#define CHARPT(x,i)	((char*)CHAR(STRING_ELT(x,i)))

/* post_run_py.c

   This program performs post-processing following execution of the PathLinker 'run.py'
   code:
   
   1. Loads the .cyjs file for the network being analyzed (node and edge information).
   
   2. Loads the list of source and target nodes ('in.txt'). This is only for sake of
      determining whether all of the specified source and target nodes actually
	  exist on the network. Those not found are written out to 'run_py_out.txt',
	  along with a tally of source and target nodes that were found.
   
   3. Loads the 'run.py'-generated list of the detected source to target paths.
   
   4. Marks each node and edge that are on one of the detected paths.
   
   5. Writes out 
   
   So basically it tries to detect and report errors that occurred in the network
   analysis, and create a new network file that is a subset of the original, for
   display of the source to target paths that were found by 'run.py'. The text
   report on the paths is placed in 'run_py_out.txt', and the new network in
   'run_py_out.cyjs'.
   
*/

#define  TRUE                     (1)
#define  FALSE                    (0)

#define  MAX_LINE_LEN             (4096)

#define  IS_NONE                  (0)
#define  IS_NODES                 (1)
#define  IS_EDGES                 (2)

struct node_record {
	int id;
	char shared_name[48];
	int isExcludedFromPaths;
	char name[48];
	int isInPath;
	double FoldChange;
	int SUID;
	char Layer[48];
	int Prize;
	int selected;
	double x;
	double y;
	int output;
    struct node_record * next;
};

struct edge_record {
	int id;
	int source;
	int target;
	char shared_name[48];
	char sh_interaction[48];
	char name[48];
	char interaction[48];
    int isInPath;
	int SUID;
	char Time[48];
	int selected;
	int output;
    struct edge_record * next;
};

struct out_path {
    int id;
	char node_name[16];
	struct out_path * next_in_path;
	struct out_path * next_path;
};

/* Yep, I used a couple of global variables, sue me. This is a simple program,
   not a giant group project. */
int num_source;
int num_target;

void print_usage( void ) {
	printf( "Usage: post_run_py <k> <Cytoscape .js file>\n\n" );
	printf( "where <k> is the user-specified limit on the number of paths\n" );
	printf( "to report (shortest paths are reported first) and the Cytoscape\n" );
	printf( ".js file contains the network information.\n" );
}

/* Add a line to the text output. Note that it is always appended; the
   program relies on another program to remove the previous 'run_py_out.txt'
   file.
*/
int write_out_message( char * message ) {
    FILE * out_file;

	out_file = fopen( "run_py_out.txt", "a" );
	
	if (out_file == NULL) {
		printf( "could not open \"run_py_out.txt\".\n" );
		return -1;
	}
	
	fprintf( out_file, "%s\n", message );
	
	fclose( out_file );
	return 0;
}

/* The 'in.txt' file is read purely for error checking. Which nodes did
   the user specify incorrectly? How many source and target nodes were
   specified correctly? Report this in the 'run_py_out.txt' file.
*/
int process_in_nodes( struct node_record * head_all_nodes ) {
	FILE * in_txt;
	char line[MAX_LINE_LEN+1];
	char node_name[400];
	char node_role[400];
    int line_num;
	struct node_record * cur_node;
	
	in_txt = fopen( "in.txt", "r" );
	
	if (in_txt == NULL) {
		printf( "unable to open 'in.txt'\n" );
		write_out_message( "Unable to open 'in.txt', please report this." );
		return 1;
	}

	num_source = 0;
	num_target = 0;

	for (line_num = 1; fgets( line, sizeof(line), in_txt ) != NULL; line_num++) {
		if (line[0] == '#')
		    /* skip comment lines */
			continue;
			
		if (sscanf( line, "%s %s", node_name, node_role ) != 2) {
    		printf( "bad line %d 'in.txt': %s", line_num, line );
	    	write_out_message( "Unable to read 'in.txt', please report this." );
		    return 1;
    	}
		
        for (cur_node = head_all_nodes; cur_node != NULL; cur_node = cur_node->next) {
			if (strcmp( cur_node->name, node_name ) == 0)
				break;
		}
		
		if (cur_node == NULL) {
			sprintf( line, "Node %s was not found in the network.", node_name );
			write_out_message( line );
		}
		else {
			if (strncmp( node_role, "source", 6 ) == 0)
				num_source += 1;
			else
			    num_target += 1;
        }
			   	   
		line_num += 1;
	}
	
	sprintf( line, "%d source %s and %d target %s were specified and found.\n",
	         num_source, (num_source != 1) ? "nodes" : "node",
			 num_target, (num_target != 1) ? "nodes" : "node" );
	write_out_message( line );

	return 0;
}

/* The format of a path: A|B|C|etc where each letter represents a node name.
   This function extracts them one at a time and advances the string pointer.
*/
int chomp_node_name( char * node_str, int start_idx, char * node_name ) {
	int i;
	
	for (i = 0; node_str[i] != '\0' && node_str[i] != '|' && node_str[i] != '\n' && node_str[i] != '\r'; i++) {
		if (i < 15)
			node_name[i] = node_str[i];
	}
	
	if (i >= 16)
		node_name[15] = '\0';
	else
		node_name[i] = '\0';
		
	if (node_str[i] == '|')
		return start_idx + i + 1;
	else
		return -1;
}

/* Load 'run_py's output of interest, namely the list of one or more paths traced
   from source nodes to target nodes.
*/
struct out_path * load_out_paths( char * paths_file_name ) {
    FILE * out_paths;
    char node_name[100][16];
	char line[MAX_LINE_LEN+1];
	int i;
	int line_num;
	int idx;
	struct out_path * cur_out_path;
	struct out_path * head_out_path;
	struct out_path * new_out_path;
	
	out_paths = fopen( paths_file_name, "r" );
	
	if (out_paths == NULL) {
		printf( "unable to open '%s'\n", paths_file_name );
		
		if (num_source != 0 || num_target != 0) {
			write_out_message( "Path analysis was not completed successfully. Possibly there were" );
			write_out_message( "no paths between the specified source(s) and target(s).\n" );
		}
		else
			write_out_message( "Path analysis was not completed successfully." );

		return NULL;
	}
	
	head_out_path = NULL;
	
	for (line_num = 1; fgets( line, sizeof(line), out_paths ) != NULL; line_num++) {
		if (line[0] == '#')
			continue;

		/* not interested in the first two fields */
		i = 0;

        while (!isspace(line[i])) {
			if (line[i] == '\0') goto bad_line;
			
			i++;
		}

    	if (line[i] == '\0') goto bad_line;
		
		while (isspace(line[i])) {
			if (line[i] == '\0') goto bad_line;
			
			i++;
		}
		
		if (line[i] == '\0') goto bad_line;

        while (!isspace(line[i])) {
			if (line[i] == '\0') goto bad_line;
			
			i++;
		}

		if (line[i] == '\0') goto bad_line;
		
		while (isspace(line[i])) {
			if (line[i] == '\0') goto bad_line;

			i++;
		}
		
		if (line[i] == '\0') goto bad_line;
		
		idx = 0;
		
		while (i > 0) {
			if (idx > 99) break;
		
			node_name[idx][0] = '\0';
		
			i = chomp_node_name( line + i, i, node_name[idx] );

			if (node_name[idx][0] != '\0')	  	  	  
				idx++;
		}
		
		if (idx == 0)
			continue;
			
		cur_out_path = NULL;

		while (idx > 0) {
			idx--;
			
			new_out_path = (struct out_path *) malloc( sizeof( struct out_path ) );
			
			strcpy( new_out_path->node_name, node_name[idx] );
			
			new_out_path->id = -1;
			new_out_path->next_in_path = cur_out_path;
			new_out_path->next_path = NULL;
			
			cur_out_path = new_out_path;
		}
		
		cur_out_path->next_path = head_out_path;
		head_out_path = cur_out_path;
    }
	
	fclose( out_paths );

	return head_out_path;

  /* goto? blasphemy! Hey, my first programming languages were Basic and Fortran,
     long before all the absolutism began. Goto statements are fine so long as
	 they're used sparingly and intelligently.
  */	
bad_line:
	fclose( out_paths );
		   	   
	printf( "bad line %d in '%s'\n", line_num, paths_file_name );
    write_out_message( "Path analysis was not completed successfully." );
	return NULL;
}

/* Flag any node and edge that appears on a path detected by 'run_py'.
*/
void flag_nodes_and_edges( struct node_record * head_node,
                           struct edge_record * head_edge,
						   struct out_path * head_out_paths )
{
	struct out_path * cur_path_head;
	struct out_path * cur_path_node;
    struct node_record * cur_node;
	struct edge_record * cur_edge;

    /* walk through the nodes first */	  
	cur_path_head = head_out_paths;
	cur_path_node = head_out_paths;

	while (cur_path_node != NULL) {
		cur_node = head_node;
		
		while (cur_node != NULL) {

			if (strcmp( cur_path_node->node_name, cur_node->name ) == 0) {
			    /* the id will be needed to test against edges */
				cur_path_node->id = cur_node->id;
				/* flag this node for output */
				cur_node->output = TRUE;
				break;
			}

			cur_node = cur_node->next;
		}
	   	   
		if (cur_path_node->next_in_path != NULL)
			cur_path_node = cur_path_node->next_in_path;
		else {
			cur_path_head = cur_path_head->next_path;
			cur_path_node = cur_path_head;
		}
	}

    /* now walk the edges */
	cur_path_head = head_out_paths;
	cur_path_node = head_out_paths;
	
	while (cur_path_node != NULL) {
		if (cur_path_node->next_in_path != NULL) {	  	  
			cur_edge = head_edge;
			
    		while (cur_edge != NULL) {
				if ((cur_edge->source == cur_path_node->id &&
				     cur_edge->target == cur_path_node->next_in_path->id))
				{
				    /* flag this edge for output */
					cur_edge->output = TRUE;
					break;
				}
				
				cur_edge = cur_edge->next;
			}
		}

		if (cur_path_node->next_in_path != NULL)
			cur_path_node = cur_path_node->next_in_path;
		else {
			cur_path_head = cur_path_head->next_path;
			cur_path_node = cur_path_head;
		}
    } 
}

/* search for a node by name
*/
struct node_record * find_node( struct node_record * head_node, int id ) {
	struct node_record * cur_node;
	
	for (cur_node = head_node; cur_node != NULL; cur_node = cur_node->next)
		if (cur_node->id == id)
			return cur_node;
	
	return NULL;
}

/* search for an edge by its source node id and target node id
*/
struct edge_record * find_edge (struct edge_record * head_edge, int from_id, int to_id ) {
	struct edge_record * cur_edge;
	
	for (cur_edge = head_edge; cur_edge != NULL; cur_edge = cur_edge->next)
		if (cur_edge->source == from_id && cur_edge->target == to_id)
			return cur_edge;

	return NULL;
}

/* just output formatting
*/
void write_arrow( char * s, int len ) {
	int i;
	
	for (i = 0; i < len + 3 + 3; i++)
		s[i] = '-';
		
	s[len + 6] = '>';
	s[len + 7] = '>';
	s[len + 8] = '\0';
}

/* Write out the detected paths in text form to 'run_py_out.txt'. The format is name of the node in the path,
   the category of its function if known, then the type of interaction with its target and the times a
   notable change occurred.
*/
void write_paths( struct node_record * head_node, struct edge_record * head_edge, struct out_path * head_out_path ) {
    struct out_path * cur_out_path_head;
	struct out_path * cur_out_path_node;
	struct node_record * cur_node;
	struct edge_record * cur_edge;
    char   line[MAX_LINE_LEN+1];
    char   arrow[MAX_LINE_LEN+1];
	int    top_interaction_len;
	int    top_time_len;

    /* determine what the longest combination (in terms of characters) of the interaction type and the time
	   information exists for all of the nodes of all of the paths, for formatting purposes */
	top_interaction_len = 0;
    top_time_len = 0;

	for (cur_out_path_head = head_out_path; cur_out_path_head != NULL; cur_out_path_head = cur_out_path_head->next_path) {
		for (cur_out_path_node = cur_out_path_head; cur_out_path_node != NULL; cur_out_path_node = cur_out_path_node->next_in_path) {
			if (cur_out_path_node->next_in_path != NULL) {
		    	cur_edge = find_edge( head_edge, cur_out_path_node->id, cur_out_path_node->next_in_path->id );
				
				if (cur_edge != NULL) {
					if (strlen( cur_edge->interaction ) > top_interaction_len)
					    top_interaction_len = strlen( cur_edge->interaction );
						
					if (strlen( cur_edge->Time ) > top_time_len )
					    top_time_len = strlen( cur_edge->Time );
				}
			}
		}
	}

    /* have an arrow of consistent length for the whole text report (wooooo!) */
	write_arrow( arrow, top_interaction_len + top_time_len + 2 );

    /* now write out each path in turn, node by node */	         
	for (cur_out_path_head = head_out_path; cur_out_path_head != NULL; cur_out_path_head = cur_out_path_head->next_path) {
		for (cur_out_path_node = cur_out_path_head; cur_out_path_node != NULL; cur_out_path_node = cur_out_path_node->next_in_path) {
    		cur_node = find_node( head_node, cur_out_path_node->id );
	   	   	   
			if (cur_node == NULL) {
				/* this shouldn't happen, but anyhow.. */
				sprintf( line, "%-15s %-15s %s", "?", "?", arrow );
				write_out_message( line );
			}
			else {
				line[0] = '\0';
			
				if (cur_out_path_node->next_in_path != NULL) {
			    	cur_edge = find_edge( head_edge, cur_out_path_node->id, cur_out_path_node->next_in_path->id );

                    if (cur_edge != NULL) {
                    	sprintf( line, "%-15s %-15s %s", cur_node->name, cur_node->Layer, arrow );
						strncpy( line + 35, cur_edge->interaction, strlen( cur_edge->interaction ) );
						line[35 + strlen( cur_edge->interaction )] = ':';
						line[36 + strlen( cur_edge->interaction )] = ' ';
						strncpy( line + 35 + strlen( cur_edge->interaction) + 2, cur_edge->Time, strlen( cur_edge->Time ) );
					}
                }
				
				if (line[0] == '\0')					
                    sprintf( line, "%-15s %-15s", cur_node->name, cur_node->Layer ); 
				
				write_out_message( line );
			}
		}

		write_out_message( " " ); 
	}
}

int post_run_py( int argc, char ** argv ) {
    FILE * db;
	FILE * out_cyjs;
    int    i;
	int    k;
	int    line_num;
    char   line[MAX_LINE_LEN+1];
	int    cur_state;
	struct node_record * cur_node;
	struct node_record * new_node;
	struct edge_record * cur_edge;
	struct edge_record * new_edge;
	struct node_record * head_node;
	struct edge_record * head_edge;
	struct out_path * head_out_path;
	int    ival;
	double dval;
	int    need_braces_line;
	int    num_nodes;
	int    num_edges;
	int    ret;
	
	if (argc != 3) {
		print_usage();
		return -1;
	}
	
	db = fopen( argv[2], "r" );
	
	if (db == NULL) {
		printf( "could not open \"%s\".\n", argv[2] );
		write_out_message( "Unable to open the network file." );
		return -1;
	}
	
    line_num = 1;
	cur_state = IS_NONE;
	cur_node = NULL;
	cur_edge = NULL;
	num_nodes = 0;
	num_edges = 0;

	/* Load the nodes and edges of the network file. Since the set of network files won't
	   change for this application, the header section is ignored. When a subset network
	   file is generated, the header is just printed out as it appears in the network
	   file, without needing to have read it.
    */
	for (line_num = 1; fgets( line, sizeof(line), db ) != NULL; line_num++) {
		if (line[0] == '#')
			continue;

        i = 0;
			
		while (line[i] != '\0') {
			if (!(isspace(line[i])))
				break;
		
			i++; 
		}
		
		if (line[i] == '\0')
			continue;
			
		if (strncmp( "\"nodes\"", line + i, 7 ) == 0) {
			cur_state = IS_NODES;
			continue;
		}
		
		if (strncmp( "\"edges\"", line + i, 7 ) == 0) {
			cur_state = IS_EDGES;
			continue;
		}
		
		if (cur_state == IS_NONE)
			continue;

		if (cur_state == IS_NODES) {			
        	if (strncmp( "\"id\"", line + i, 4 ) == 0) {
    			new_node = (struct node_record *) malloc( sizeof( struct node_record ) );
			
				if (sscanf( line + i + 8, "%d", &ival ) == 0) {
					printf( "failed to read id on line %d, line was '%s'\n", line_num, line );
            		write_out_message( "Unable to read the network file (bad format)." );
	   	   			return -1;
				}
			
				new_node->id = ival;
				new_node->shared_name[0] = '\0';
				new_node->isExcludedFromPaths = FALSE;
				new_node->name[0] = '\0';
				new_node->isInPath = FALSE;
				new_node->FoldChange = 0.0;
				new_node->SUID = 0;
				new_node->Layer[0] = '\0';
				new_node->Prize = 0;
				new_node->selected = FALSE;
				new_node->output = FALSE;
				
				new_node->next = cur_node;
				cur_node = new_node;
				
				num_nodes += 1;
				
				continue;
			}
			
    		if (strncmp( "\"shared_name\"", line + i, 13 ) == 0) {
				if (cur_node == NULL) {
					printf( "node field before id? line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
	   	   			return -1;
				}
				
                for (k = 17; line[i + k] != '"'; k++) {
					if (k - 17 >= 15)
						break;
				
					cur_node->shared_name[k - 17] = line[i + k];
				}
				
				cur_node->shared_name[k - 17] = '\0';
				
    			continue;
			}
			
			if (strncmp( "\"isExcludedFromPaths\"", line + i, 21) == 0) {
				if (cur_node == NULL) {
					printf( "node field before id? line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
	   	   			return -1;
				}
				
    			if (strncmp( "false", line + i + 24, 5) == 0)
					cur_node->isExcludedFromPaths = FALSE;
				else if (strncmp( "true", line + i + 24, 4) == 0)
					cur_node->isExcludedFromPaths = TRUE;
				else {
					printf( "value is neither true nor false: line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
	   	   			return -1;	       	    			
				}
				
				continue;
	   	   	}
			
    		if (strncmp( "\"name\"", line + i, 6 ) == 0) {
				if (cur_node == NULL) {
					printf( "node field before id? line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
				
                for (k = 10; line[i + k] != '"'; k++) {
					if (k - 10 >= 15)
						break;
				
					cur_node->name[k - 10] = line[i + k];
				}
				
				cur_node->name[k - 10] = '\0';
				
    			continue;
			}
			
			if (strncmp( "\"isInPath\"", line + i, 10) == 0) {
				if (cur_node == NULL) {
					printf( "node field before id? line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
				
    			if (strncmp( "false", line + i + 13, 5) == 0)
					cur_node->isInPath = FALSE;
				else if (strncmp( "true", line + i + 13, 4) == 0)
					cur_node->isInPath = TRUE;
				else {
					printf( "value is neither true nor false: line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;	       	    			
				}
				
				continue;
	   	   	}
			
        	if (strncmp( "\"FoldChange\"", line + i, 12 ) == 0) {
				if (cur_node == NULL) {
					printf( "node field before id? line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
				
    			if (sscanf( line + i + 15, "%lf", &dval ) == 0) {
					printf( "failed to read dval on line %d, line was '%s'\n", line_num, line );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
			
				cur_node->FoldChange = dval;
				
				continue;
			}
			
        	if (strncmp( "\"SUID\"", line + i, 6 ) == 0) {
				if (cur_node == NULL) {
					printf( "node field before id? line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
				
    			if (sscanf( line + i + 9, "%d", &ival ) == 0) {
					printf( "failed to read ival on line %d, line was '%s'\n", line_num, line );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
			
				cur_node->SUID = ival;
				
				continue;
			}

    		if (strncmp( "\"Layer\"", line + i, 7 ) == 0) {
				if (cur_node == NULL) {
					printf( "node field before id? line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
				
                for (k = 11; line[i + k] != '"'; k++) {
					if (k - 11 >= 15)
						break;
				
					cur_node->Layer[k - 11] = line[i + k];
				}
				
				cur_node->Layer[k - 11] = '\0';
				
    			continue;
			}
			
            if (strncmp( "\"Prize\"", line + i, 7 ) == 0) {
				if (cur_node == NULL) {
					printf( "node field before id? line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
				
    			if (sscanf( line + i + 10, "%d", &ival ) == 0) {
					printf( "failed to read ival on line %d, line was '%s'\n", line_num, line );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
			
				cur_node->Prize = ival;
				
				continue;
			}

			if (strncmp( "\"selected\"", line + i, 10) == 0) {
				if (cur_node == NULL) {
					printf( "node field before id? line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
				
    			if (strncmp( "false", line + i + 13, 5) == 0)
					cur_node->selected = FALSE;
				else if (strncmp( "true", line + i + 13, 4) == 0)
					cur_node->selected = TRUE;
				else {
					printf( "value is neither true nor false: line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;	       	    			
				}
				
				continue;
	   	   	}
			
            if (strncmp( "\"x\"", line + i, 3 ) == 0) {
				if (cur_node == NULL) {
					printf( "node field before id? line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
				
    			if (sscanf( line + i + 6, "%lf", &dval ) == 0) {
					printf( "failed to read dval on line %d, line was '%s'\n", line_num, line );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
			
				cur_node->x = dval;
				
				continue;
			}

            if (strncmp( "\"y\"", line + i, 3 ) == 0) {
				if (cur_node == NULL) {
					printf( "node field before id? line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
				
    			if (sscanf( line + i + 6, "%lf", &dval ) == 0) {
					printf( "failed to read dval on line %d, line was '%s'\n", line_num, line );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
			
				cur_node->y = dval;
				
				continue;
			}
			
    	   	continue;
        }
		
		if (cur_state == IS_EDGES) {			
        	if (strncmp( "\"id\"", line + i, 4 ) == 0) {
    			new_edge = (struct edge_record *) malloc( sizeof( struct edge_record ) );
			
				if (sscanf( line + i + 8, "%d", &ival ) == 0) {
					printf( "failed to read id on line %d, line was '%s'\n", line_num, line );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
			
				new_edge->id = ival;
				new_edge->source = 0;
				new_edge->target = 0;
				new_edge->shared_name[0] = '\0';
				new_edge->sh_interaction[0] = '\0';
				new_edge->name[0] = '\0';
				new_edge->interaction[0] = '\0';
				new_edge->isInPath = FALSE;
				new_edge->SUID = 0;
				new_edge->Time[0] = '\0';
				new_edge->selected = FALSE;
				new_edge->output = FALSE;
 	 	 	 
				new_edge->next = cur_edge;
				cur_edge = new_edge;
				
				num_edges += 1;
				
				continue;
			}
			
        	if (strncmp( "\"source\"", line + i, 8 ) == 0) {
				if (cur_edge == NULL) {
					printf( "edge field before id? line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
				
    			if (sscanf( line + i + 12, "%d", &ival ) == 0) {
					printf( "failed to read ival on line %d, line was '%s'\n", line_num, line );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
			
				cur_edge->source = ival;
				
				continue;
			}

        	if (strncmp( "\"target\"", line + i, 8 ) == 0) {
				if (cur_edge == NULL) {
					printf( "edge field before id? line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
				
    			if (sscanf( line + i + 12, "%d", &ival ) == 0) {
					printf( "failed to read ival on line %d, line was '%s'\n", line_num, line );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
			
				cur_edge->target = ival;
				
				continue;
			}

       	    if (strncmp( "\"shared_name\"", line + i, 13 ) == 0) {
				if (cur_edge == NULL) {
					printf( "edge field before id? line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
				
                for (k = 17; line[i + k] != '"'; k++) {
					if (k - 17 >= 47)
						break;
				
					cur_edge->shared_name[k - 17] = line[i + k];
				}
				
				cur_edge->shared_name[k - 17] = '\0';
				
    			continue;
			}
			
       	    if (strncmp( "\"sh_interaction\"", line + i, 16 ) == 0) {
				if (cur_edge == NULL) {
					printf( "edge field before id? line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
				
                for (k = 20; line[i + k] != '"'; k++) {
					if (k - 20 >= 47)
						break;
				
					cur_edge->sh_interaction[k - 20] = line[i + k];
				}
				
				cur_edge->sh_interaction[k - 20] = '\0';
				
    			continue;
			}
			
 	    	if (strncmp( "\"name\"", line + i, 6 ) == 0) {
				if (cur_edge == NULL) {
					printf( "edge field before id? line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
				
                for (k = 10; line[i + k] != '"'; k++) {
					if (k - 10 >= 47)
						break;
				
					cur_edge->name[k - 10] = line[i + k];
				}
				
				cur_edge->name[k - 10] = '\0';
				
    			continue;
			}
			
       	    if (strncmp( "\"interaction\"", line + i, 13 ) == 0) {
				if (cur_edge == NULL) {
					printf( "edge field before id? line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
				
                for (k = 17; line[i + k] != '"'; k++) {
					if (k - 17 >= 47)
						break;
				
					cur_edge->interaction[k - 17] = line[i + k];
				}
				
				cur_edge->interaction[k - 17] = '\0';
				
    			continue;
			}
			
    		if (strncmp( "\"isInPath\"", line + i, 10) == 0) {
				if (cur_edge == NULL) {
					printf( "edge field before id? line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
				
    			if (strncmp( "false", line + i + 13, 5) == 0)
					cur_edge->isInPath = FALSE;
				else if (strncmp( "true", line + i + 13, 4) == 0)
					cur_edge->isInPath = TRUE;
				else {
					printf( "value is neither true nor false: line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;	       	    			
				}
				
				continue;
	   	   	}
			
        	if (strncmp( "\"SUID\"", line + i, 6 ) == 0) {
				if (cur_edge == NULL) {
					printf( "edge field before id? line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
				
    			if (sscanf( line + i + 9, "%d", &ival ) == 0) {
					printf( "failed to read ival on line %d, line was '%s'\n", line_num, line );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
			
				cur_edge->SUID = ival;
				
				continue;
			}

    		if (strncmp( "\"Time\"", line + i, 6 ) == 0) {
				if (cur_edge == NULL) {
					printf( "edge field before id? line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
				
                for (k = 10; line[i + k] != '"'; k++) {
					if (k - 10 >= 47)
						break;
				
					cur_edge->Time[k - 10] = line[i + k];
				}
				
				cur_edge->Time[k - 10] = '\0';
				
    			continue;
			}
			
			if (strncmp( "\"selected\"", line + i, 10) == 0) {
				if (cur_edge == NULL) {
					printf( "edge field before id? line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;
				}
				
    			if (strncmp( "false", line + i + 13, 5) == 0)
					cur_edge->selected = FALSE;
				else if (strncmp( "true", line + i + 13, 4) == 0)
					cur_edge->selected = TRUE;
				else {
					printf( "value is neither true nor false: line %d\n", line_num );
            		write_out_message( "Unable to read the network file (bad format)." );
    				return -1;	       	    			
				}
				
				continue;
	   	   	}
		}
    }

	fclose( db );
	
	printf( "post_run_py: loaded '%s'; num_nodes=%d num_edges=%d\n", argv[2], num_nodes, num_edges );
	
	head_node = cur_node;
	head_edge = cur_edge;

    /* Load 'in.txt' */
	printf( "post_run_py: loading 'in.txt'\n" );
	if((ret=process_in_nodes( head_node )))
		return ret;

    /* Load the list of detected path. The file name will depend on the limit specified
	   for the number of paths to be recorded ('k').
	*/
	printf( "post_run_py: loading detected paths from '%s'\n", argv[1] );
	if(!(head_out_path = load_out_paths( argv[1] )))
		return 1;

    /* Mark the nodes and edges that are part of the paths detected by 'run.py'.*/
	printf( "post_run_py: marking nodes and edges that are part of the detected paths\n" );
    flag_nodes_and_edges( head_node, head_edge, head_out_path );

    /* Write out the subset of the network for display. */ 
	printf( "post_run_py: writing out network subset to 'run_py_out.cyjs'\n" );
	out_cyjs = fopen( "run_py_out.cyjs", "w" );
	
	if (out_cyjs == NULL) {
		printf( "could not open 'run_py_out.cyjs'\n" );
		write_out_message( "Unable to open the sub-network file to write." );
		return -1;
	}
	
    fprintf( out_cyjs, "{\n" );
    fprintf( out_cyjs, "  \"format_version\" : \"1.0\",\n" );
    fprintf( out_cyjs, "  \"generated_by\" : \"cytoscape-3.7.2\",\n" );
    fprintf( out_cyjs, "  \"target_cytoscapejs_version\" : \"~2.1\",\n" );
    fprintf( out_cyjs, "  \"data\" : {\n" );
    fprintf( out_cyjs, "    \"shared_name\" : \"my_time1_all_edge.txt\",\n" );
    fprintf( out_cyjs, "    \"name\" : \"my_time1_all_edge.txt\",\n" );
    fprintf( out_cyjs, "    \"SUID\" : 81000,\n" );
    fprintf( out_cyjs, "    \"__Annotations\" : [ \"\" ],\n" );
    fprintf( out_cyjs, "    \"selected\" : true\n" );
    fprintf( out_cyjs, "  },\n" );
    fprintf( out_cyjs, "  \"elements\" : {\n" );
	fprintf( out_cyjs, "    \"nodes\" : [ {\n" );
	
	need_braces_line = FALSE;

	while (cur_node != NULL) {
		if (cur_node->output == FALSE) {
			cur_node = cur_node->next;
			continue;
		}
		
		if (need_braces_line == TRUE)
			fprintf( out_cyjs, "    }, {\n" );
	
		fprintf( out_cyjs, "      \"data\" : {\n" );
		fprintf( out_cyjs, "        \"id\" : \"%d\",\n", cur_node->id );
		fprintf( out_cyjs, "        \"shared_name\" : \"%s\",\n", cur_node->shared_name );
		fprintf( out_cyjs, "        \"isExcludedFromPaths\" : %s,\n",
		         (cur_node->isExcludedFromPaths == TRUE ? "true" : "false") );
	    fprintf( out_cyjs, "        \"name\" : \"%s\",\n", cur_node->name );
		fprintf( out_cyjs, "        \"isInPath\" : %s,\n",
		         (cur_node->isInPath == TRUE ? "true" : "false") );
        fprintf( out_cyjs, "        \"FoldChange\" : %lf,\n", cur_node->FoldChange );
		fprintf( out_cyjs, "        \"SUID\" : %d,\n", cur_node->SUID );
		fprintf( out_cyjs, "        \"Layer\" : \"%s\",\n", cur_node->Layer );
		fprintf( out_cyjs, "        \"Prize\" : %d,\n", cur_node->Prize );
		fprintf( out_cyjs, "        \"selected\" : %s\n",
     	         (cur_node->selected == TRUE ? "true" : "false"));
	    fprintf( out_cyjs, "      },\n" );
		fprintf( out_cyjs, "      \"position\" : {\n" );
		fprintf( out_cyjs, "        \"x\" : %lf,\n", cur_node->x );
		fprintf( out_cyjs, "        \"y\" : %lf\n", cur_node->y );
		fprintf( out_cyjs, "      },\n" );
		fprintf( out_cyjs, "      \"selected\" : %s\n",
		        (cur_node->selected == TRUE ? "true" : "false"));
			   
		need_braces_line = TRUE;

		cur_node = cur_node->next;
	}

	fprintf( out_cyjs, "    } ],\n" );
	fprintf( out_cyjs, "    \"edges\" : [ {\n" );
	
	need_braces_line = FALSE;

	while (cur_edge != NULL) {
		if (cur_edge->output == FALSE) {
			cur_edge = cur_edge->next;
			continue;
		}
	
		if (need_braces_line == TRUE)
			fprintf( out_cyjs, "    }, {\n" );
	
 	    fprintf( out_cyjs, "      \"data\" : {\n" );
		fprintf( out_cyjs, "        \"id\" : \"%d\",\n", cur_edge->id );
		fprintf( out_cyjs, "        \"source\" : \"%d\",\n", cur_edge->source );
		fprintf( out_cyjs, "        \"target\" : \"%d\",\n", cur_edge->target );
    	fprintf( out_cyjs, "        \"shared_name\" : \"%s\",\n", cur_edge->shared_name );
    	fprintf( out_cyjs, "        \"sh_interaction\" : \"%s\",\n", cur_edge->sh_interaction );
	    fprintf( out_cyjs, "        \"name\" : \"%s\",\n", cur_edge->name );
		fprintf( out_cyjs, "        \"interaction\" : \"%s\",\n", cur_edge->interaction );
		fprintf( out_cyjs, "        \"isInPath\" : %s,\n",
		         (cur_edge->isInPath == TRUE ? "true" : "false") );
		fprintf( out_cyjs, "        \"SUID\" : %d,\n", cur_edge->SUID );
		fprintf( out_cyjs, "        \"Time\" : \"%s\",\n", cur_edge->Time );
		fprintf( out_cyjs, "        \"selected\" : %s\n",
     	         (cur_edge->selected == TRUE ? "true" : "false"));
	    fprintf( out_cyjs, "      },\n" );
		fprintf( out_cyjs, "      \"selected\" : %s\n",
		        (cur_edge->selected == TRUE ? "true" : "false"));

		need_braces_line = TRUE;
			
		cur_edge = cur_edge->next;
	}
	
	fprintf( out_cyjs, "    } ]\n" );
    fprintf( out_cyjs, "  }\n" );
	fprintf( out_cyjs, "}\n" );
	
	fflush( out_cyjs );
	fclose( out_cyjs );

    /* Create a text format report of the detected paths that contains more detail
	   than that produced by 'run.py'.
	*/
	printf( "post_run_py: writing text report to 'run_py_out.txt'\n" );
	write_paths( head_node, head_edge, head_out_path );

    printf( "post_run_py: complete\n" );
	return 0;
}

SEXP R_post_run_py(SEXP R_args){
	int i;
	int argc=length(R_args);
	char **argv;

	argv=malloc(sizeof(*argv)*argc);
	if(argv==NULL)
		return NULL;
	for(i=0;i<argc;i++){
		argv[i]=CHARPT(R_args, i);
	}

	post_run_py(argc,argv);

	free(argv);

	return NULL;
}
