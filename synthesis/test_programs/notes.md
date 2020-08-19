its Apply(name, keys, actions, default_action)
and each csv line is an edit
the third column is a list of ;-separated matches that correspond to the keys of the table
the fourth column is the action data, and the 5th column is the action index
the slash is standard longest prefix matching syntax and the hash denotes the size of the bitvector