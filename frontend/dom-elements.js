function getCurrentBoard() {
    const queryString = window.location.search;
    const urlParams = new URLSearchParams(queryString);
    return urlParams.get('board');
}

function columnHeaderAsElement(column) {

    ul = document.createElement('ul');
    ul.id = 'list_' + column.name;
    ul.classList.add('ticket-column');

    li = document.createElement('li');

    h2 = document.createElement('h2');
    h2.id = column.id;
    h2.addEventListener('dragover', columnHeaderAllowDrop);
    h2.addEventListener('drop', columnHeaderDropTicket);

    h2.classList.add('noselect');
    h2.textContent = column.name; // TODO pretty name

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

function buildColumnHeaders(board) {
    const tickets = document.getElementById('tickets');
    tickets.textContent = '';
    for (const columnNo in board) {
        tickets.appendChild(columnHeaderAsElement(board[columnNo]));
    }
}
