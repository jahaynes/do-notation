function columnHeaderAsElement(column) {

    ul = document.createElement('ul');
    ul.id = 'list_' + column.name;
    ul.classList.add('ticket-column');

    li = document.createElement('li');

    h2 = document.createElement('h2');
    h2.id = column.columnid;
    h2.addEventListener('dragover', columnHeaderAllowDrop);
    h2.addEventListener('drop', columnHeaderDropTicket);

    h2.classList.add('noselect');
    h2.textContent = column.name;

    li.appendChild(h2);
    ul.appendChild(li);
    return ul;
}

function ticketAsElement(columnId, jsonTicket) {

    li = document.createElement('li');
    li.id = jsonTicket.id;
    li.columnId = columnId;
    li.classList.add('ticket-li');
    li.draggable = 'true';
    li.addEventListener('dragstart', ticketOnDragStart);
    li.addEventListener('click', ticketSelect);
    
    p = document.createElement('p');
    p.classList.add('noselect');
    p.classList.add('noclick');
    p.textContent = jsonTicket.content;

    li.appendChild(p);
    return li;
}

function buildColumnHeaders(columns) {
    const tickets = document.getElementById('tickets');
    tickets.textContent = '';
    for (const columnNo in columns) {
        tickets.appendChild(columnHeaderAsElement(columns[columnNo]));
    }
}

function buildBoards(boardIdsAndNames) {

    const boards = document.getElementById('boards');
    boards.textContent = '';

    const ul = document.createElement('ul');   

    for(const bian in boardIdsAndNames) {
        const boardId = boardIdsAndNames[bian][0];
        const boardName = boardIdsAndNames[bian][1];
        ul.appendChild(boardAsElement(boardId, boardName));
    }

    boards.appendChild(ul);
}

function boardAsElement(boardId, boardName) {
    li = document.createElement('li');
    
    const button = document.createElement('button');
    button.id = boardId;
    button.addEventListener('click', boardSelect);
    button.textContent = boardName;

    li.appendChild(button);
    return li;
}
