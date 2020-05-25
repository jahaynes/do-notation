function ticketAsElement(columnId, jsonTicket) {

    const li = document.createElement('li');
    li.id = jsonTicket.id;
    li.columnId = columnId;
    li.classList.add('ticket-li');
    li.draggable = 'true';
    li.addEventListener('dragstart', ticketOnDragStart);
    li.addEventListener('click', ticketSelect);

    const p = document.createElement('p');
    p.classList.add('noselect');
    p.classList.add('noclick');
    p.textContent = jsonTicket.content;

    li.appendChild(p);
    return li;
}

function buildBoards(restApi) {

    const boardAsElementImpl = boardAsElement(restApi);

    const newBoardAsElementImpl = newBoardAsElement(restApi);

    return async(boardIdsAndNames) => {
        const boards = document.getElementById('boards');
        boards.textContent = '';

        const ul = document.createElement('ul');   
        ul.appendChild(await newBoardAsElementImpl());

        for(const bian in boardIdsAndNames) {
            const boardId = boardIdsAndNames[bian][0];
            const boardName = boardIdsAndNames[bian][1];
            ul.appendChild(await boardAsElementImpl(boardId, boardName));
        }

        boards.appendChild(ul);
    };
}

function boardAsElement(restApi) {

    const boardSelectByInputImpl = boardSelectByInput(restApi);

    return async(boardId, boardName) => {
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

    return async () => {

        const li = document.createElement('li');  

        const button = document.createElement('button');
        button.id = "btnNewBoard";
        button.addEventListener('click', buttonNewBoardClick);
        button.textContent = "+ new +";
        button.focus();

        const input = document.createElement('input');
        input.id = 'newBoardInput';
        input.addEventListener('blur', newBoardInputBlurImpl);
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

    return async () => {

        const input = document.getElementById('newBoardInput');
        const boardId = input.value;
        input.value = '';
        input.classList.add('hidden');

        const button = document.getElementById('btnNewBoard');
        button.disabled = false;

        restApi.createBoard(boardId);
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

    return async(boardId) => {
        document.getElementById('boards').activeBoardId = boardId;
        restApi.withBoard(boardId, async (board) => {
            await buildColumnHeadersImpl(board.columns);
            restApi.getColumns(board.columns);
        });
    };
}

function buildColumnHeaders(restApi) {

    const columnHeaderAsElementImpl = columnHeaderAsElement(restApi);

    const deleteBoardButtonImpl = deleteBoardButton(restApi);

    return async(columns) => {
        const tickets = document.getElementById('tickets');
        tickets.textContent = '';
        for (const columnNo in columns) {
            tickets.appendChild(await columnHeaderAsElementImpl(columns[columnNo]));
        }
        tickets.appendChild(await deleteBoardButtonImpl());
    };
}

function columnHeaderAsElement(restApi) {

    const columnHeaderDropTicketImpl = columnHeaderDropTicket(restApi);

    return async(column) => {
        const ul = document.createElement('ul');
        ul.id = 'list_' + column.name;
        ul.classList.add('ticket-column');

        const li = document.createElement('li');

        const h2 = document.createElement('h2');
        h2.id = column.columnid;
        h2.addEventListener('dragover', columnHeaderAllowDrop);
        h2.addEventListener('drop', columnHeaderDropTicketImpl);
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
    return async (event) => {
        event.preventDefault();
        const fromId   = event.dataTransfer.getData('from')
        const toId     = event.target.id;  
        const ticketId = event.dataTransfer.getData('ticket')
        clearTicketPanel();
        setVisible(panels.NONE);
        restApi.moveTicket(getCurrentBoard(), fromId, toId, ticketId);
    };
}

function deleteBoardButton(restApi) {

    const deleteCurrentBoardImpl = deleteCurrentBoard(restApi);

    return async() => {

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
        console.log(boardId);
        restApi.deleteBoard(boardId);
    }
}