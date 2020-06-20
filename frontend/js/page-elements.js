function ticketAsElement(restApi) {

    const ticketSelectImpl = ticketSelect(restApi);

    return (columnId, jsonTicket) => {

        const li = document.createElement('li');
        li.id = jsonTicket.id;
        li.columnId = columnId;
        li.classList.add('ticket-li');
        li.draggable = 'true';
        li.addEventListener('dragstart', ticketOnDragStart);
        li.addEventListener('click', ticketSelectImpl);

        const p = document.createElement('p');
        p.classList.add('noselect');
        p.classList.add('noclick');
        p.textContent = jsonTicket.content;

        li.appendChild(p);
        return li;
    };
}

function buildBoards(restApi) {

    const boardAsElementImpl = boardAsElement(restApi);

    const newBoardAsElementImpl = newBoardAsElement(restApi);

    return async(boardIdsAndNames) => {
        const boards = document.getElementById('boards');
        boards.textContent = '';

        const ul = document.createElement('ul');   
        ul.appendChild(newBoardAsElementImpl());

        for(const bian in boardIdsAndNames) {
            const boardId = boardIdsAndNames[bian][0];
            const boardName = boardIdsAndNames[bian][1];
            ul.appendChild(boardAsElementImpl(boardId, boardName));
        }

        boards.appendChild(ul);
    };
}

function boardAsElement(restApi) {

    const boardSelectByInputImpl = boardSelectByInput(restApi);

    return (boardId, boardName) => {
        const li = document.createElement('li');

        const button = document.createElement('button');
        button.id = boardId;
        button.addEventListener('click', boardSelectByInputImpl);
        button.textContent = boardName;

        li.appendChild(button);
        return li;
    };
}

function newBoardAsElement(restApi) {

    const newBoardInputBlurImpl = newBoardInputBlur(restApi);

    return () => {

        const li = document.createElement('li');  

        const button = document.createElement('button');
        button.id = "btnNewBoard";
        button.addEventListener('click', buttonNewBoardClick);
        button.textContent = "+ new +";
        button.focus();

        const input = document.createElement('input');
        input.id = 'newBoardInput';
        input.addEventListener('blur', newBoardInputBlurImpl);
        input.addEventListener('keypress', function(event) {
            if(event.which == 10 || event.which == 13) {
                input.blur();
            }
        });

        input.classList.add('hidden');

        button.appendChild(input);
        li.appendChild(button);
        return li;
    };
}

function buttonNewBoardClick() {

    const button = document.getElementById('btnNewBoard');
    button.disabled = true;

    const input = document.getElementById('newBoardInput');
    input.classList.remove('hidden');
    input.focus();
}

function newBoardInputBlur(restApi) {

    const boardSelectByIdImpl = boardSelectById(restApi);

    return async () => {

        const input = document.getElementById('newBoardInput');
        const boardName = input.value.trim();
        input.value = '';
        input.classList.add('hidden');

        const button = document.getElementById('btnNewBoard');
        button.disabled = false;

        if(boardName) {
            const result = await restApi.createBoard(boardName);
            if(result.success) {
                getBoards(restApi);
                boardSelectByIdImpl(result.boardId);
            }
        }
    };
}

function boardSelectByInput(restApi) {

    const boardSelectByIdImpl = boardSelectById(restApi);

    return async(event) => {
        boardSelectByIdImpl(event.target.id);
    };
}

function boardSelectById(restApi) {

    const buildColumnHeadersImpl = buildColumnHeaders(restApi);

    const ticketAsElementImpl = ticketAsElement(restApi);

    return async(boardId) => {

        if (boardId) {

            setCurrentBoard(boardId);

            document.getElementById('input-board-id').value = boardId;
            document.getElementById('btn-show-share-board').removeAttribute('disabled');

            restApi.withBoard(boardId, async (board) => {

                const columns = board.columns;

                await buildColumnHeadersImpl(columns);
                restApi.withColumns(columns, async(columnNo, column) => {

                    const columnName = document.getElementById('list_' + columns[columnNo].name);

                    const columnId = columns[columnNo].columnid;

                    for (const ticketNo in column) {
                        columnName.appendChild(ticketAsElementImpl(columnId, column[ticketNo]));
                    }
                });
            });
        }
    };
}

function buildColumnHeaders(restApi) {

    const columnHeaderAsElementImpl = columnHeaderAsElement(restApi);

    const deleteBoardButtonImpl = deleteBoardButton(restApi);

    return async(columns) => {
        const tickets = document.getElementById('tickets');
        tickets.textContent = '';
        for (const columnNo in columns) {
            tickets.appendChild(columnHeaderAsElementImpl(columns[columnNo]));
        }
        tickets.appendChild(deleteBoardButtonImpl());
    };
}

function columnHeaderAsElement(restApi) {

    return column => {
        const ul = document.createElement('ul');
        ul.id = 'list_' + column.name;
        ul.classList.add('ticket-column');

        const li = document.createElement('li');

        const h2 = document.createElement('h2');
        h2.id = column.columnid;
        h2.addEventListener('dragover', columnHeaderAllowDrop);
        h2.addEventListener('drop', columnHeaderDropTicket(restApi));
        h2.classList.add('noselect');
        h2.classList.add('ticket-header');
        h2.textContent = column.name;

        li.appendChild(h2);
        ul.appendChild(li);
        return ul;
    };
}

function columnHeaderAllowDrop(event) {
    event.preventDefault();
}

function columnHeaderDropTicket(restApi) {

    const boardSelectByIdImpl = boardSelectById(restApi);

    return async (event) => {
        event.preventDefault();
        const fromId   = event.dataTransfer.getData('from')
        const toId     = event.target.id;  
        const ticketId = event.dataTransfer.getData('ticket')
        setVisible(panels.NONE);

        // Move ticket & refresh page
        const boardId = getCurrentBoard();
        await restApi.moveTicket(boardId, fromId, toId, ticketId);
        boardSelectByIdImpl(boardId);
    };
}

function deleteBoardButton(restApi) {

    const deleteCurrentBoardImpl = deleteCurrentBoard(restApi);

    return () => {

        const ul = document.createElement('ul');
        ul.classList.add('ticket-column');

        const li = document.createElement('li');

        const button = document.createElement('button');
        button.id = 'btnDeleteCurrentBoard';
        button.textContent = 'x Delete Board x';
        button.classList.add('ticket-header');
        button.classList.add('ticket-header-delete');
        button.addEventListener('click', deleteCurrentBoardImpl);

        li.appendChild(button);
        ul.appendChild(li);
        return ul;
    };
}

function deleteCurrentBoard(restApi) {
    return async() => {
        const boardId = getCurrentBoard();
        const success = await restApi.deleteBoard(boardId);
        if(success) {
            location = location;
        }
    }
}
