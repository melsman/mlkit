structure IdentFinMap = OrderFinMap(struct type T = Ident.id
					   fun lt id1 (id2:T) = Ident.<(id1,id2)
				    end)
