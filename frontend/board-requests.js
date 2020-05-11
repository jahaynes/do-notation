
const moveTicket =
    async (boardName, from, to, ticket) => {
        const body = { 'board': boardName
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

        fetchBoard(boardName);
    }

const createTicket = 
    async (boardName, name, content) => {
        const body = { 'board': boardName
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

        fetchBoard(boardName);
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
    }

const restDeleteTicket = 
    async (boardName, columnId, ticketId) => {
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
        fetchBoard(boardName);
    }

const fetchBoard =
    async (boardName, authFailHandler) => {
        const response = await fetch('/board?board=' + boardName);

        switch (response.status) {

            case 200:
                const board = await response.json();
                buildColumnHeaders(board);
                fetchColumns(board, authFailHandler);
                break;

            case 401:
                authFailHandler();
                break;
        }
    }

const fetchColumns =
    async (board, authFailHandler) => {

        for (const columnNo in board) {
            
            let err = false;
            const columnId = board[columnNo].id;

            const response = await fetch('/column?columnId=' + columnId);

            switch(response.status) {

                case 401:
                    err = true;
                    authFailHandler();
                    break;

                case 200:
                    const column   = await response.json();
                    const columnName = document.getElementById('list_' + board[columnNo].name);
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
