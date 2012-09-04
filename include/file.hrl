-record(upload_state, {
		 filename
		,content_type
		,uploader :: pid()
		,files = []
		,size = 0
	}).
