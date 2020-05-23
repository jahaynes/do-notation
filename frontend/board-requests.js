
const moveTicket =
    async (boardId, from, to, ticket) => {
        const body = { 'board': boardId
                     , 'from': from
                     , 'to': to
                     , 'ticket': ticket
                    };
        const response =
            await fetch('/ticket/move', {
                method: 'POST',
                body: JSON.stringify(body),
                headers: {
                    'Content-Type': 'application/json'
                }
            });

        fetchBoard(boardId);
    }

const createTicket = 
    async (boardId, name, content) => {
        const body = { 'boardId': boardId
                     , 'name': name
                     , 'content': content
                     };
        const response =
            await fetch('/ticket', {
                method: 'POST',
                body: JSON.stringify(body),
                headers: {
                    'Content-Type': 'application/json',
                }
            });

        fetchBoard(boardId);
    }

const restUpdateTicket =
    async (columnId, ticketId, name, content) => {
        const body = { 'columnId': columnId
                     , 'ticketId': ticketId
                     , 'name': name
                     , 'content': content
                     };
        const response =
            await fetch('/ticket', {
                method: 'PUT',
                body: JSON.stringify(body),
                headers: {
                    'Content-Type': 'application/json',
                }
            });

        //TODO check status
    }

const restDeleteTicket = 
    async (boardId, columnId, ticketId) => {
        const body = { 'columnId': columnId
                     , 'ticketId': ticketId
                     };
        const response =
            await fetch('/ticket', {
                method: 'DELETE',
                body: JSON.stringify(body),
                headers: {
                    'Content-Type': 'application/json'
                }
            });

        //TODO check status
        fetchBoard(boardId);
    }

const fetchBoardsForUser = //TODO check status, add authFailHandler
    async () => {
        const response = await fetch('/boards');
        // console.log(response.status);
        const body = await response.json();
        buildBoards(body);
    }

const fetchBoard =
    async (boardId, authFailHandler) => {
        const response = await fetch('/board?board=' + boardId);

        switch (response.status) {

            case 200:
                const board = await response.json();
                buildColumnHeaders(board.columns);
                fetchColumns(board.columns, authFailHandler);
                break;

            case 401:
                authFailHandler();
                break;
        }
    }

const restCreateBoard = 
    async (boardName) => {
        const response =
            await fetch('/board', {
                method: 'POST',
                body: '"' + boardName + '"',
                headers: {
                    'Content-Type': 'application/json'
                }
            });

        switch(response.status) {

            case 401:
                //TODO
                console.log("auth error");
                break;

            case 200:
                const boardId = await response.json();
                fetchBoardsForUser();
        }
    }

const fetchColumns =
    async (columns, authFailHandler) => {

        for (const columnNo in columns) {
            
            let err = false;
            const columnId = columns[columnNo].columnid;

            const response = await fetch('/column?columnId=' + columnId);

            switch(response.status) {

                case 401:
                    err = true;
                    authFailHandler();
                    break;

                case 200:
                    const column = await response.json();
                    const columnName = document.getElementById('list_' + columns[columnNo].name);
                    for (const ticketNo in column) {
                        columnName.appendChild(ticketAsElement(columnId, column[ticketNo]));
                    }
            }

            if(err) {
                break;
            }
        }
    }

const fetchTicket =
    async (columnId, ticketId, f) => {
        const response = await fetch('/ticket?columnId=' + columnId + "&ticketId=" + ticketId);
        const json = await response.json();
        f(json);
    }
