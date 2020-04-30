
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
            await fetch('/ticket/create', {
                method: 'POST',
                body: JSON.stringify(body),
                headers: {
                    'Content-Type': 'application/json'
                }
            });

        fetchBoard(boardName);
    }

const fetchBoard =
    async (boardName) => {
        const response = await fetch('/board?board=' + boardName);
        const board = await response.json();
        buildColumnHeaders(board);
        fetchColumns(board);
    }

const fetchColumns =
    async (board) => {
        for (const columnNo in board) {
            const columnId = board[columnNo].id;
            const response = await fetch('/column?columnId=' + columnId);
            const column   = await response.json();
            const columnName = document.getElementById('list_' + board[columnNo].name);
            for (const ticketNo in column) {
                columnName.appendChild(ticketAsElement(columnId, column[ticketNo]));
            }
        }
    }

const fetchTicket =
    async (columnId, ticketId, f) => {
        const response = await fetch('/ticket/get?columnId=' + columnId + "&ticketId=" + ticketId);
        const json = await response.json();
        f(json);
    }