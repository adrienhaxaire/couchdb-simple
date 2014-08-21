SimpleCouch
============

A simple CouchDB library for Haskell. It is a thin layer on top of CouchDB's HTTP API.

Example:
--------

    ghci> let mydb = Data.Maybe.fromJust $ db "http://127.0.0.1:5984/test"
	ghci> putDB mydb
	"{\"ok\":true}\n"

	ghci> getDBInfo mydb
	"{\"db_name\":\"test\",\"doc_count\":0,\"doc_del_count\":0,\"update_seq\":0,\"purge_seq\":0,\"compact_running\":false,\"disk_size\":79,\"data_size\":0,\"instance_start_time\":\"1408660456711906\",\"disk_format_version\":6,\"committed_update_seq\":0}\n"

	ghci> deleteDB mydb
    "{\"ok\":true}\n"

	ghci> getDBInfo mydb
	"{\"error\":\"not_found\",\"reason\":\"no_db_file\"}\n"
